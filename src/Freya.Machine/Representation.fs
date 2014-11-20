[<AutoOpen>]
module internal Freya.Machine.Representation

open System.Globalization
open Freya.Core
open Freya.Core.Operators
open Freya.Typed

(* Aliases *)

module H = Response.Headers

(* Negotiation *)

let private createNegotiation charsets encodings mediaTypes languages =
    { Charsets = charsets
      Encodings = encodings
      MediaTypes = mediaTypes
      Languages = languages }

let private negotiate =
        createNegotiation
    <!> Charset.negotiated
    <*> Encoding.negotiated
    <*> MediaType.negotiated
    <*> Language.negotiated

(* Metadata *)

let private charset =
    function | Some (c: Charset) -> modPLM H.contentType id
             | _ -> returnM ()

let private encodings =
    function | Some (es: Encoding list) -> setPLM H.contentEncoding (ContentEncoding es)
             | _ -> returnM ()

let private mediaType =
    function | Some (m: MediaType) -> setPLM H.contentType (ContentType m)
             | _ -> returnM ()

// TODO: Instate after ContentLanguage Header Completed

//let private languages =
//    function | Some (ls: CultureInfo list) -> setPLM H.contentLanguage (ContentLanguage ls)
//             | _ -> returnM ()

let private metadata (m: FreyaMachineRepresentationMetadata) =
       mediaType m.MediaType 
    *> charset m.Charset
    *> encodings m.Encodings
//    *> languages m.Languages

(* Data *)

let private data (d: byte []) =
       setPLM H.contentLength (ContentLength d.Length)
    *> modLM Response.body (fun b -> b.Write (d, 0, d.Length); b)

(* Representation *)

let private representation (r: FreyaMachineRepresentation) =
        metadata r.Metadata 
    >>. data r.Data

let represent (handler: FreyaMachineHandler) =
        negotiate 
    >>= handler 
    >>= representation