[<AutoOpen>]
module internal Freya.Machine.Representation

open System.Globalization
open Freya.Core
open Freya.Core.Operators
open Freya.Types
open Freya.Types.Http

(* Negotiation *)

let private negotiation charsets encodings mediaTypes languages =
    { Charsets = charsets
      Encodings = encodings
      MediaTypes = mediaTypes
      Languages = languages }

let private negotiate =
        negotiation
    <!> Charset.negotiated
    <*> Encoding.negotiated
    <*> MediaType.negotiated
    <*> Language.negotiated

(* Metadata *)

let private charset =
    function | Some x -> modPLM Response.Headers.contentType id
             | _ -> returnM ()

let private encodings =
    function | Some x -> setPLM Response.Headers.contentEncoding (ContentEncoding x)
             | _ -> returnM ()

let private mediaType =
    function | Some x -> setPLM Response.Headers.contentType (ContentType x)
             | _ -> returnM ()

let private languages =
    function | Some x -> setPLM Response.Headers.contentLanguage (ContentLanguage x)
             | _ -> returnM ()

let private metadata (metadata: FreyaMachineRepresentationMetadata) =
        mediaType metadata.MediaType 
     *> charset metadata.Charset
     *> encodings metadata.Encodings
     *> languages metadata.Languages

(* Data *)

let private data (data: byte []) =
        setPLM Response.Headers.contentLength (ContentLength data.Length)
     *> modLM Response.body (fun b -> b.Write (data, 0, data.Length); b)

(* Representation *)

let private representation (representation: FreyaMachineRepresentation) =
        metadata representation.Metadata 
    >>. data representation.Data

let represent (handler: FreyaMachineHandler) =
        negotiate 
    >>= handler 
    >>= representation