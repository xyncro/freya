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
//
//----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module internal Freya.Machine.Extensions.Http.Handlers

open Aether
open Aether.Operators
open Arachne.Http
open Freya.Core
open Freya.Core.Operators
open Freya.Lenses.Http
open Freya.Machine

(* Negotiation *)

let private charsetsNegotiated config =
    ContentNegotiation.Charset.negotiated
        (Freya.Lens.get Request.Headers.acceptCharset_)
        ((Configuration.get Properties.CharsetsSupported >> Option.orElse Defaults.charsetsSupported) config)

let private encodingsNegotiated config =
    ContentNegotiation.Encoding.negotiated
        (Freya.Lens.get Request.Headers.acceptEncoding_)
        ((Configuration.get Properties.EncodingsSupported >> Option.orElse Defaults.encodingsSupported) config)

let private mediaTypesNegotiated config =
    ContentNegotiation.MediaType.negotiated
        (Freya.Lens.get Request.Headers.accept_)
        ((Configuration.get Properties.MediaTypesSupported >> Option.orElse Defaults.mediaTypesSupported) config)

let private languagesNegotiated config =
    ContentNegotiation.Language.negotiated
        (Freya.Lens.get Request.Headers.acceptLanguage_)
        ((Configuration.get Properties.LanguagesSupported >> Option.orElse Defaults.languagesSupported) config)

(* Specification *)

let private specification config =
        fun c e m l ->
            { Charsets = c
              Encodings = e
              MediaTypes = m
              Languages = l }
    <!> charsetsNegotiated config
    <*> encodingsNegotiated config
    <*> mediaTypesNegotiated config
    <*> languagesNegotiated config

(* Representation *)

let private charset_ =
        Response.Headers.contentType_
     >- Option.mapIsomorphism ContentType.MediaType_
     >- Option.mapLens MediaType.Parameters_
     >? Parameters.Parameters_
     >? Map.value_ "charset"

let private charset =
        Option.map (fun (Charset x) -> charset_ .?= Some x)
     >> Option.orElse (Freya.init ())

let private encodings =
        Option.map (fun x -> Response.Headers.contentEncoding_ .= Some (ContentEncoding x))
     >> Option.orElse (Freya.init ())

let private mediaType =
        Option.map (fun x -> Response.Headers.contentType_ .= Some (ContentType x))
     >> Option.orElse (Freya.init ())

let private languages =
        Option.map (fun x -> Response.Headers.contentLanguage_ .= Some (ContentLanguage x))
     >> Option.orElse (Freya.init ())

let private description x =
        mediaType x.MediaType
     *> charset x.Charset
     *> encodings x.Encodings
     *> languages x.Languages

let private data x =
    Response.body_ %= (fun body -> body.Write (x, 0, x.Length); body)

(* Handlers *)

let private handle config m =
    let specification = specification config

    freya {
        let! meth = Freya.Lens.get Request.method_
        let! specification = specification
        let! representation = m specification

        do! description representation.Description

        (* NOTE:

           This is a temporary solution to the issue of returning body
           data in HEAD requests. This should be fixed longer term with an
           elaboration of the HTTP graph, but for now, this is a fairly safe
           and harmless fix before later work on Freya optimised HTTP
           graphs. *)

        match meth with
        | HEAD -> return ()
        | _ -> do! data representation.Data }

let private userHandler key =
    Some (Compile (fun config ->
        Configuration.get key config
        |> Option.map (fun m -> Compiled (Unary (handle config m), configured))
        |> Option.orElse (Compiled (Unary (Freya.init ()), unconfigured))))

(* Graph *)

open Freya.Machine.Operators

let operations =
    [ Operation Handlers.Accepted                      =.        userHandler Handlers.Accepted
      Operation Handlers.BadRequest                    =.        userHandler Handlers.BadRequest
      Operation Handlers.Conflict                      =.        userHandler Handlers.Conflict
      Operation Handlers.Created                       =.        userHandler Handlers.Created
      Operation Handlers.Forbidden                     =.        userHandler Handlers.Forbidden
      Operation Handlers.Gone                          =.        userHandler Handlers.Gone
      Operation Handlers.MethodNotAllowed              =.        userHandler Handlers.MethodNotAllowed
      Operation Handlers.MovedPermanently              =.        userHandler Handlers.MovedPermanently
      Operation Handlers.MovedTemporarily              =.        userHandler Handlers.MovedTemporarily
      Operation Handlers.MultipleRepresentations       =.        userHandler Handlers.MultipleRepresentations
      Operation Handlers.NoContent                     =.        userHandler Handlers.NoContent
      Operation Handlers.NotAcceptable                 =.        userHandler Handlers.NotAcceptable
      Operation Handlers.NotFound                      =.        userHandler Handlers.NotFound
      Operation Handlers.NotImplemented                =.        userHandler Handlers.NotImplemented
      Operation Handlers.NotModified                   =.        userHandler Handlers.NotModified
      Operation Handlers.OK                            =.        userHandler Handlers.OK
      Operation Handlers.Options                       =.        userHandler Handlers.Options
      Operation Handlers.PreconditionFailed            =.        userHandler Handlers.PreconditionFailed
      Operation Handlers.RequestEntityTooLarge         =.        userHandler Handlers.RequestEntityTooLarge
      Operation Handlers.SeeOther                      =.        userHandler Handlers.SeeOther
      Operation Handlers.ServiceUnavailable            =.        userHandler Handlers.ServiceUnavailable
      Operation Handlers.Unauthorized                  =.        userHandler Handlers.Unauthorized
      Operation Handlers.UnknownMethod                 =.        userHandler Handlers.UnknownMethod
      Operation Handlers.UnprocessableEntity           =.        userHandler Handlers.UnprocessableEntity
      Operation Handlers.UnsupportedMediaType          =.        userHandler Handlers.UnsupportedMediaType
      Operation Handlers.UriTooLong                    =.        userHandler Handlers.UriTooLong

      Operation Handlers.Accepted                      >.        Finish
      Operation Handlers.BadRequest                    >.        Finish
      Operation Handlers.Conflict                      >.        Finish
      Operation Handlers.Created                       >.        Finish
      Operation Handlers.Forbidden                     >.        Finish
      Operation Handlers.Gone                          >.        Finish
      Operation Handlers.MethodNotAllowed              >.        Finish
      Operation Handlers.MovedPermanently              >.        Finish
      Operation Handlers.MovedTemporarily              >.        Finish
      Operation Handlers.MultipleRepresentations       >.        Finish
      Operation Handlers.NoContent                     >.        Finish
      Operation Handlers.NotAcceptable                 >.        Finish
      Operation Handlers.NotFound                      >.        Finish
      Operation Handlers.NotImplemented                >.        Finish
      Operation Handlers.NotModified                   >.        Finish
      Operation Handlers.OK                            >.        Finish
      Operation Handlers.Options                       >.        Finish
      Operation Handlers.PreconditionFailed            >.        Finish
      Operation Handlers.RequestEntityTooLarge         >.        Finish
      Operation Handlers.SeeOther                      >.        Finish
      Operation Handlers.ServiceUnavailable            >.        Finish
      Operation Handlers.Unauthorized                  >.        Finish
      Operation Handlers.UnknownMethod                 >.        Finish
      Operation Handlers.UnprocessableEntity           >.        Finish
      Operation Handlers.UnsupportedMediaType          >.        Finish
      Operation Handlers.UriTooLong                    >.        Finish ]