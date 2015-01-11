//----------------------------------------------------------------------------
//
// Copyright (c) 2014
//
//    Ryan Riley (@panesofglass) and Andrew Cherry (@kolektiv)
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//----------------------------------------------------------------------------

[<AutoOpen>]
module internal Freya.Machine.Representation

open Freya.Core
open Freya.Core.Operators
open Freya.Types.Http

(* Negotiation *)

let private negotiate =
        (fun c e m l -> 
            { Charsets = c
              Encodings = e
              MediaTypes = m
              Languages = l })
    <!> Charset.Negotiation.negotiated
    <*> Encoding.Negotiation.negotiated
    <*> MediaType.Negotiation.negotiated
    <*> Language.Negotiation.negotiated

(* Metadata *)

// TODO: Set the Charset on the MediaType!

let private charset =
    function | Some x -> modPLM Response.Headers.contentType id
             | _ -> Freya.returnM ()

let private encodings =
    function | Some x -> setPLM Response.Headers.contentEncoding (ContentEncoding x)
             | _ -> Freya.returnM ()

let private mediaType =
    function | Some x -> setPLM Response.Headers.contentType (ContentType x)
             | _ -> Freya.returnM ()

let private languages =
    function | Some x -> setPLM Response.Headers.contentLanguage (ContentLanguage x)
             | _ -> Freya.returnM ()

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
