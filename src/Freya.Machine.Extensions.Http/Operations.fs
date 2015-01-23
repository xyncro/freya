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
module internal Freya.Machine.Extensions.Http.Operations

open System
open Freya.Core
open Freya.Core.Operators
open Freya.Machine
open Freya.Machine.Operators
open Freya.Types.Http

(* Helpers *)

let allow =
       tryGetConfig Configuration.MethodsSupported
    >> Option.map (fun x -> setPLM Response.Headers.allow =<< (Allow <!> x))
    >> Option.orElse (setPLM Response.Headers.allow =<< (Allow <!> Defaults.methodsSupported))

let date =
    setPLM Response.Headers.date (Date.Date DateTime.UtcNow)

let eTag =
       tryGetConfig Configuration.ETag
    >> Option.map (fun x -> setPLM Response.Headers.expires =<< (Expires <!> x))
    >> Option.orElse (Freya.init ())

let expires =
       tryGetConfig Configuration.Expires 
    >> Option.map (fun x -> setPLM Response.Headers.expires =<< (Expires <!> x))
    >> Option.orElse (Freya.init ())

let lastModified =
       tryGetConfig Configuration.LastModified 
    >> Option.map (fun x -> setPLM Response.Headers.lastModified =<< (LastModified <!> x))
    >> Option.orElse (Freya.init ())

let location =
       tryGetConfig Configuration.Location
    >> Option.map (fun x -> setPLM Response.Headers.location =<< (Location <!> x))
    >> Option.orElse (Freya.init ())

let phrase =
    setPLM Response.reasonPhrase

let status =
    setPLM Response.statusCode

(* Operations *)

let systemOperation f =
    Unary (fun c ->
        unconfigurable, f c)

let accepted _ =
       status 202
    *> phrase "Accepted"
    *> date

let badRequest _ =
       status 400
    *> phrase "Bad Request"
    *> date

let conflict _ =
       status 409
    *> phrase "Conflict"
    *> date

let created config =
       status 201
    *> phrase "Created"
    *> date
    *> location config

let forbidden _ =
       status 403
    *> phrase "Forbidden"
    *> date

let gone _ =
       status 410 
    *> phrase "Gone"
    *> date

let methodNotAllowed config =
       status 405
    *> phrase "Method Not Allowed"
    *> allow config
    *> date

let movedPermanently config =
       status 301
    *> phrase "Moved Permanently"
    *> date
    *> location config

let movedTemporarily config =
       status 307
    *> phrase "Moved Temporarily"
    *> date
    *> location config

let multipleRepresentations _ =
       status 310
    *> phrase "Multiple Representations"
    *> date

let noContent _ =
       status 204
    *> phrase "No Content"
    *> date

let notAcceptable _ =
       status 406
    *> phrase "Not Acceptable"
    *> date

let notFound _ =
       status 404
    *> phrase "Not Found"
    *> date

let notImplemented _ =
       status 501
    *> phrase "Not Implemented"
    *> date

let notModified config =
       status 304
    *> phrase "Not Modified"
    *> lastModified config
    *> date
    *> eTag config
    *> expires config

let ok config =
       status 200
    *> phrase "OK"
    *> lastModified config
    *> date
    *> eTag config
    *> expires config

let options config =
       status 200
    *> phrase "Options"
    *> lastModified config
    *> date
    *> eTag config
    *> expires config

let preconditionFailed _ =
       status 412
    *> phrase "Precondition Failed"
    *> date

let requestEntityTooLarge _ =
       status 413
    *> phrase "Request Entity Too Large"
    *> date

let seeOther config =
       status 303
    *> phrase "See Other"
    *> date
    *> location config

let serviceUnavailable _ =
       status 503
    *> phrase "Service Unavailable"
    *> date

let unauthorized _ =
       status 401
    *> phrase "Unauthorized"
    *> date

let unknownMethod _ =
       status 501
    *> phrase "Unknown Method"
    *> date

let unprocessableEntity _ =
       status 422
    *> phrase "Unprocessable Entity"
    *> date

let unsupportedMediaType _ =
       status 415
    *> phrase "UnsupportedMediaType"
    *> date

let uriTooLong _ =
       status 414
    *> phrase "URI Too Long"
    *> date

(* Graph *)

let operations =
    [ Ref Operations.Accepted                    =.        systemOperation accepted
      Ref Operations.BadRequest                  =.        systemOperation badRequest
      Ref Operations.Conflict                    =.        systemOperation conflict
      Ref Operations.Created                     =.        systemOperation created
      Ref Operations.Forbidden                   =.        systemOperation forbidden
      Ref Operations.Gone                        =.        systemOperation gone
      Ref Operations.MethodNotAllowed            =.        systemOperation methodNotAllowed
      Ref Operations.MovedPermanently            =.        systemOperation movedPermanently
      Ref Operations.MovedTemporarily            =.        systemOperation movedTemporarily
      Ref Operations.MultipleRepresentations     =.        systemOperation multipleRepresentations
      Ref Operations.NoContent                   =.        systemOperation noContent
      Ref Operations.NotAcceptable               =.        systemOperation notAcceptable
      Ref Operations.NotFound                    =.        systemOperation notFound
      Ref Operations.NotImplemented              =.        systemOperation notImplemented
      Ref Operations.NotModified                 =.        systemOperation notModified
      Ref Operations.OK                          =.        systemOperation ok
      Ref Operations.Options                     =.        systemOperation options
      Ref Operations.PreconditionFailed          =.        systemOperation preconditionFailed
      Ref Operations.RequestEntityTooLarge       =.        systemOperation requestEntityTooLarge
      Ref Operations.SeeOther                    =.        systemOperation seeOther
      Ref Operations.ServiceUnavailable          =.        systemOperation serviceUnavailable
      Ref Operations.Unauthorized                =.        systemOperation unauthorized
      Ref Operations.UnknownMethod               =.        systemOperation unknownMethod
      Ref Operations.UnprocessableEntity         =.        systemOperation unprocessableEntity
      Ref Operations.UnsupportedMediaType        =.        systemOperation unsupportedMediaType
      Ref Operations.UriTooLong                  =.        systemOperation uriTooLong

      Ref Operations.Accepted                    >.        Ref Handlers.Accepted
      Ref Operations.BadRequest                  >.        Ref Handlers.BadRequest
      Ref Operations.Conflict                    >.        Ref Handlers.Conflict
      Ref Operations.Created                     >.        Ref Handlers.Created
      Ref Operations.Forbidden                   >.        Ref Handlers.Forbidden
      Ref Operations.Gone                        >.        Ref Handlers.Gone
      Ref Operations.MethodNotAllowed            >.        Ref Handlers.MethodNotAllowed
      Ref Operations.MovedPermanently            >.        Ref Handlers.MovedPermanently
      Ref Operations.MovedTemporarily            >.        Ref Handlers.MovedTemporarily
      Ref Operations.MultipleRepresentations     >.        Ref Handlers.MultipleRepresentations
      Ref Operations.NoContent                   >.        Ref Handlers.NoContent
      Ref Operations.NotAcceptable               >.        Ref Handlers.NotAcceptable
      Ref Operations.NotFound                    >.        Ref Handlers.NotFound
      Ref Operations.NotImplemented              >.        Ref Handlers.NotImplemented
      Ref Operations.NotModified                 >.        Ref Handlers.NotModified
      Ref Operations.OK                          >.        Ref Handlers.OK
      Ref Operations.Options                     >.        Ref Handlers.Options
      Ref Operations.PreconditionFailed          >.        Ref Handlers.PreconditionFailed
      Ref Operations.RequestEntityTooLarge       >.        Ref Handlers.RequestEntityTooLarge
      Ref Operations.SeeOther                    >.        Ref Handlers.SeeOther
      Ref Operations.ServiceUnavailable          >.        Ref Handlers.ServiceUnavailable
      Ref Operations.Unauthorized                >.        Ref Handlers.Unauthorized
      Ref Operations.UnknownMethod               >.        Ref Handlers.UnknownMethod
      Ref Operations.UnprocessableEntity         >.        Ref Handlers.UnprocessableEntity
      Ref Operations.UnsupportedMediaType        >.        Ref Handlers.UnsupportedMediaType
      Ref Operations.UriTooLong                  >.        Ref Handlers.UriTooLong ]