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
open Freya.Core
open Freya.Core.Operators
open Freya.Machine
open Freya.Machine.Operators
open Freya.Types.Http

(* Negotiation *)

let private charsetsNegotiated config =
    ContentNegotiation.Charset.negotiated
        (getPLM Request.Headers.acceptCharset)
        (tryGetConfiguration Configuration.CharsetsSupported config |> Option.orElse Defaults.charsetsSupported)

let private encodingsNegotiated config =
    ContentNegotiation.Encoding.negotiated
        (getPLM Request.Headers.acceptEncoding)
        (tryGetConfiguration Configuration.EncodingsSupported config |> Option.orElse Defaults.encodingsSupported)

let private mediaTypesNegotiated config =
    ContentNegotiation.MediaType.negotiated
        (getPLM Request.Headers.accept)
        (tryGetConfiguration Configuration.MediaTypesSupported config |> Option.orElse Defaults.mediaTypesSupported)

let private languagesNegotiated config =
    ContentNegotiation.Language.negotiated
        (getPLM Request.Headers.acceptLanguage)
        (tryGetConfiguration Configuration.LanguagesSupported config |> Option.orElse Defaults.languagesSupported)

(* Specification *)

let private specification config =
        (fun c e m l ->
            { Charsets = c
              Encodings = e
              MediaTypes = m
              Languages = l })
    <!> charsetsNegotiated config
    <*> encodingsNegotiated config
    <*> mediaTypesNegotiated config
    <*> languagesNegotiated config

(* Representation *)

let private charsetPLens =
        Response.Headers.contentType
   >?-> ContentType.MediaTypeLens
   >?-> MediaType.ParametersLens
   >??> mapPLens "charset"

let private charset =
    function | Some (Charset x) -> setPLM charsetPLens x
             | _ -> Freya.init ()

let private encodings =
    function | Some x -> setPLM Response.Headers.contentEncoding (ContentEncoding x)
             | _ -> Freya.init ()

let private mediaType =
    function | Some x -> setPLM Response.Headers.contentType (ContentType x)
             | _ -> Freya.init ()

let private languages =
    function | Some x -> setPLM Response.Headers.contentLanguage (ContentLanguage x)
             | _ -> Freya.init ()

let private description x =
        mediaType x.MediaType
     *> charset x.Charset
     *> encodings x.Encodings
     *> languages x.Languages

let private data x =
    modLM Response.body (fun b -> b.Write (x, 0, x.Length); b)

(* Handlers *)

let private handle config m =
    let specification = specification config

    freya {
        let! specification = specification
        let! representation = m specification

        do! description representation.Description
        do! data representation.Data }

let private userHandler key =
    Unary (fun config ->
        match tryGetConfiguration key config with
        | Some m -> configured, handle config m
        | _ -> unconfigured, Freya.init ())

(* Graph *)

let operations =
    [ Ref Handlers.Accepted                            =.        userHandler Handlers.Accepted
      Ref Handlers.BadRequest                          =.        userHandler Handlers.BadRequest
      Ref Handlers.Conflict                            =.        userHandler Handlers.Conflict
      Ref Handlers.Created                             =.        userHandler Handlers.Created
      Ref Handlers.Forbidden                           =.        userHandler Handlers.Forbidden
      Ref Handlers.Gone                                =.        userHandler Handlers.Gone
      Ref Handlers.MethodNotAllowed                    =.        userHandler Handlers.MethodNotAllowed
      Ref Handlers.MovedPermanently                    =.        userHandler Handlers.MovedPermanently
      Ref Handlers.MovedTemporarily                    =.        userHandler Handlers.MovedTemporarily
      Ref Handlers.MultipleRepresentations             =.        userHandler Handlers.MultipleRepresentations
      Ref Handlers.NoContent                           =.        userHandler Handlers.NoContent
      Ref Handlers.NotAcceptable                       =.        userHandler Handlers.NotAcceptable
      Ref Handlers.NotFound                            =.        userHandler Handlers.NotFound
      Ref Handlers.NotImplemented                      =.        userHandler Handlers.NotImplemented
      Ref Handlers.NotModified                         =.        userHandler Handlers.NotModified
      Ref Handlers.OK                                  =.        userHandler Handlers.OK
      Ref Handlers.Options                             =.        userHandler Handlers.Options
      Ref Handlers.PreconditionFailed                  =.        userHandler Handlers.PreconditionFailed
      Ref Handlers.RequestEntityTooLarge               =.        userHandler Handlers.RequestEntityTooLarge
      Ref Handlers.SeeOther                            =.        userHandler Handlers.SeeOther
      Ref Handlers.ServiceUnavailable                  =.        userHandler Handlers.ServiceUnavailable
      Ref Handlers.Unauthorized                        =.        userHandler Handlers.Unauthorized
      Ref Handlers.UnknownMethod                       =.        userHandler Handlers.UnknownMethod
      Ref Handlers.UnprocessableEntity                 =.        userHandler Handlers.UnprocessableEntity
      Ref Handlers.UnsupportedMediaType                =.        userHandler Handlers.UnsupportedMediaType
      Ref Handlers.UriTooLong                          =.        userHandler Handlers.UriTooLong

      Ref Handlers.Accepted                            >.        Finish
      Ref Handlers.BadRequest                          >.        Finish
      Ref Handlers.Conflict                            >.        Finish
      Ref Handlers.Created                             >.        Finish
      Ref Handlers.Forbidden                           >.        Finish
      Ref Handlers.Gone                                >.        Finish
      Ref Handlers.MethodNotAllowed                    >.        Finish
      Ref Handlers.MovedPermanently                    >.        Finish
      Ref Handlers.MovedTemporarily                    >.        Finish
      Ref Handlers.MultipleRepresentations             >.        Finish
      Ref Handlers.NoContent                           >.        Finish
      Ref Handlers.NotAcceptable                       >.        Finish
      Ref Handlers.NotFound                            >.        Finish
      Ref Handlers.NotImplemented                      >.        Finish
      Ref Handlers.NotModified                         >.        Finish
      Ref Handlers.OK                                  >.        Finish
      Ref Handlers.Options                             >.        Finish
      Ref Handlers.PreconditionFailed                  >.        Finish
      Ref Handlers.RequestEntityTooLarge               >.        Finish
      Ref Handlers.SeeOther                            >.        Finish
      Ref Handlers.ServiceUnavailable                  >.        Finish
      Ref Handlers.Unauthorized                        >.        Finish
      Ref Handlers.UnknownMethod                       >.        Finish
      Ref Handlers.UnprocessableEntity                 >.        Finish
      Ref Handlers.UnsupportedMediaType                >.        Finish
      Ref Handlers.UriTooLong                          >.        Finish ]