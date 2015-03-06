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
    >> Option.map (fun x -> Freya.setLensPartial Response.Headers.allow =<< (Allow <!> x))
    >> Option.orElse (Freya.setLensPartial Response.Headers.allow =<< (Allow <!> Defaults.methodsSupported))

let date =
    Freya.setLensPartial Response.Headers.date (Date.Date DateTime.UtcNow)

let eTag =
       tryGetConfig Configuration.ETag
    >> Option.map (fun x -> Freya.setLensPartial Response.Headers.expires =<< (Expires <!> x))
    >> Option.orElse (Freya.init ())

let expires =
       tryGetConfig Configuration.Expires 
    >> Option.map (fun x -> Freya.setLensPartial Response.Headers.expires =<< (Expires <!> x))
    >> Option.orElse (Freya.init ())

let lastModified =
       tryGetConfig Configuration.LastModified 
    >> Option.map (fun x -> Freya.setLensPartial Response.Headers.lastModified =<< (LastModified <!> x))
    >> Option.orElse (Freya.init ())

let location =
       tryGetConfig Configuration.Location
    >> Option.map (fun x -> Freya.setLensPartial Response.Headers.location =<< (Location <!> x))
    >> Option.orElse (Freya.init ())

let phrase =
    Freya.setLensPartial Response.reasonPhrase

let status =
    Freya.setLensPartial Response.statusCode

(* Operations *)

let systemOperation f =
    Some (Compile (fun config ->
        Compiled (Unary (f config), unconfigurable)))

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
    [ Operation Operations.Accepted                    =.        systemOperation accepted
      Operation Operations.BadRequest                  =.        systemOperation badRequest
      Operation Operations.Conflict                    =.        systemOperation conflict
      Operation Operations.Created                     =.        systemOperation created
      Operation Operations.Forbidden                   =.        systemOperation forbidden
      Operation Operations.Gone                        =.        systemOperation gone
      Operation Operations.MethodNotAllowed            =.        systemOperation methodNotAllowed
      Operation Operations.MovedPermanently            =.        systemOperation movedPermanently
      Operation Operations.MovedTemporarily            =.        systemOperation movedTemporarily
      Operation Operations.MultipleRepresentations     =.        systemOperation multipleRepresentations
      Operation Operations.NoContent                   =.        systemOperation noContent
      Operation Operations.NotAcceptable               =.        systemOperation notAcceptable
      Operation Operations.NotFound                    =.        systemOperation notFound
      Operation Operations.NotImplemented              =.        systemOperation notImplemented
      Operation Operations.NotModified                 =.        systemOperation notModified
      Operation Operations.OK                          =.        systemOperation ok
      Operation Operations.Options                     =.        systemOperation options
      Operation Operations.PreconditionFailed          =.        systemOperation preconditionFailed
      Operation Operations.RequestEntityTooLarge       =.        systemOperation requestEntityTooLarge
      Operation Operations.SeeOther                    =.        systemOperation seeOther
      Operation Operations.ServiceUnavailable          =.        systemOperation serviceUnavailable
      Operation Operations.Unauthorized                =.        systemOperation unauthorized
      Operation Operations.UnknownMethod               =.        systemOperation unknownMethod
      Operation Operations.UnprocessableEntity         =.        systemOperation unprocessableEntity
      Operation Operations.UnsupportedMediaType        =.        systemOperation unsupportedMediaType
      Operation Operations.UriTooLong                  =.        systemOperation uriTooLong

      Operation Operations.Accepted                    >.        Operation Handlers.Accepted
      Operation Operations.BadRequest                  >.        Operation Handlers.BadRequest
      Operation Operations.Conflict                    >.        Operation Handlers.Conflict
      Operation Operations.Created                     >.        Operation Handlers.Created
      Operation Operations.Forbidden                   >.        Operation Handlers.Forbidden
      Operation Operations.Gone                        >.        Operation Handlers.Gone
      Operation Operations.MethodNotAllowed            >.        Operation Handlers.MethodNotAllowed
      Operation Operations.MovedPermanently            >.        Operation Handlers.MovedPermanently
      Operation Operations.MovedTemporarily            >.        Operation Handlers.MovedTemporarily
      Operation Operations.MultipleRepresentations     >.        Operation Handlers.MultipleRepresentations
      Operation Operations.NoContent                   >.        Operation Handlers.NoContent
      Operation Operations.NotAcceptable               >.        Operation Handlers.NotAcceptable
      Operation Operations.NotFound                    >.        Operation Handlers.NotFound
      Operation Operations.NotImplemented              >.        Operation Handlers.NotImplemented
      Operation Operations.NotModified                 >.        Operation Handlers.NotModified
      Operation Operations.OK                          >.        Operation Handlers.OK
      Operation Operations.Options                     >.        Operation Handlers.Options
      Operation Operations.PreconditionFailed          >.        Operation Handlers.PreconditionFailed
      Operation Operations.RequestEntityTooLarge       >.        Operation Handlers.RequestEntityTooLarge
      Operation Operations.SeeOther                    >.        Operation Handlers.SeeOther
      Operation Operations.ServiceUnavailable          >.        Operation Handlers.ServiceUnavailable
      Operation Operations.Unauthorized                >.        Operation Handlers.Unauthorized
      Operation Operations.UnknownMethod               >.        Operation Handlers.UnknownMethod
      Operation Operations.UnprocessableEntity         >.        Operation Handlers.UnprocessableEntity
      Operation Operations.UnsupportedMediaType        >.        Operation Handlers.UnsupportedMediaType
      Operation Operations.UriTooLong                  >.        Operation Handlers.UriTooLong ]