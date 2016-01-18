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
open Arachne.Http
open Freya.Core
open Freya.Core.Operators
open Freya.Lenses.Http
open Freya.Machine

(* Helpers *)

let private allow =
       Configuration.get Properties.MethodsSupported
    >> Option.map (fun x -> (.=) Response.Headers.allow_ =<< ((Allow >> Some) <!> x))
    >> Option.orElse ((.=) Response.Headers.allow_ =<< ((Allow >> Some) <!> Defaults.methodsSupported))

let private date =
    (.=) Response.Headers.date_ (Some (Date.Date DateTime.UtcNow))

let private eTag =
       Configuration.get Properties.ETag
    >> Option.map (fun x -> (ETag >> Some) <!> x >>= (.=) Response.Headers.eTag_)
    >> Option.orElse (Freya.init ())

let private expires =
       Configuration.get Properties.Expires 
    >> Option.map (fun x -> (Expires >> Some) <!> x >>= (.=) Response.Headers.expires_)
    >> Option.orElse (Freya.init ())

let private lastModified =
       Configuration.get Properties.LastModified 
    >> Option.map (fun x -> (LastModified >> Some) <!> x >>= (.=) Response.Headers.lastModified_)
    >> Option.orElse (Freya.init ())

let private location =
       Configuration.get Properties.Location
    >> Option.map (fun x -> (Location >> Some) <!> x >>= (.=) Response.Headers.location_)
    >> Option.orElse (Freya.init ())

let private phrase =
    Some >> (.=) Response.reasonPhrase_

let private status =
    Some >> (.=) Response.statusCode_

(* Operations *)

let private systemOperation f =
    Some (Compile (fun config ->
        Compiled (Unary (f config), unconfigurable)))

let inline private ignoreConfig f _ x = f x

let private accepted =
        status 202
     *> phrase "Accepted"
     *> date

let private badRequest =
        status 400
     *> phrase "Bad Request"
     *> date

let private conflict =
        status 409
     *> phrase "Conflict"
     *> date

let private created config =
        status 201
     *> phrase "Created"
     *> date
     *> location config

let private forbidden =
        status 403
     *> phrase "Forbidden"
     *> date

let private gone =
        status 410 
     *> phrase "Gone"
     *> date

let private methodNotAllowed config =
        status 405
     *> phrase "Method Not Allowed"
     *> allow config
     *> date

let private movedPermanently config =
        status 301
     *> phrase "Moved Permanently"
     *> date
     *> location config

let private movedTemporarily config =
        status 307
     *> phrase "Moved Temporarily"
     *> date
     *> location config

let private multipleRepresentations =
        status 310
     *> phrase "Multiple Representations"
     *> date

let private noContent =
        status 204
     *> phrase "No Content"
     *> date

let private notAcceptable =
        status 406
     *> phrase "Not Acceptable"
     *> date

let private notFound =
        status 404
     *> phrase "Not Found"
     *> date

let private notImplemented =
        status 501
     *> phrase "Not Implemented"
     *> date

let private notModified config =
        status 304
     *> phrase "Not Modified"
     *> lastModified config
     *> date
     *> eTag config
     *> expires config

let private ok config =
        status 200
     *> phrase "OK"
     *> lastModified config
     *> date
     *> eTag config
     *> expires config

let private options config =
        status 200
     *> phrase "Options"
     *> lastModified config
     *> date
     *> eTag config
     *> expires config

let private preconditionFailed =
        status 412
     *> phrase "Precondition Failed"
     *> date

let private requestEntityTooLarge =
        status 413
     *> phrase "Request Entity Too Large"
     *> date

let private seeOther config =
        status 303
     *> phrase "See Other"
     *> date
     *> location config

let private serviceUnavailable =
        status 503
     *> phrase "Service Unavailable"
     *> date

let private unauthorized =
        status 401
     *> phrase "Unauthorized"
     *> date

let private unknownMethod =
        status 501
     *> phrase "Unknown Method"
     *> date

let private unprocessableEntity =
        status 422
     *> phrase "Unprocessable Entity"
     *> date

let private unsupportedMediaType =
        status 415
     *> phrase "UnsupportedMediaType"
     *> date

let private uriTooLong =
        status 414
     *> phrase "URI Too Long"
     *> date

(* Graph *)

open Freya.Machine.Operators

let operations =
    [ Operation Operations.Accepted                    =.        systemOperation (ignoreConfig accepted)
      Operation Operations.BadRequest                  =.        systemOperation (ignoreConfig badRequest)
      Operation Operations.Conflict                    =.        systemOperation (ignoreConfig conflict)
      Operation Operations.Created                     =.        systemOperation created
      Operation Operations.Forbidden                   =.        systemOperation (ignoreConfig forbidden)
      Operation Operations.Gone                        =.        systemOperation (ignoreConfig gone)
      Operation Operations.MethodNotAllowed            =.        systemOperation methodNotAllowed
      Operation Operations.MovedPermanently            =.        systemOperation movedPermanently
      Operation Operations.MovedTemporarily            =.        systemOperation movedTemporarily
      Operation Operations.MultipleRepresentations     =.        systemOperation (ignoreConfig multipleRepresentations)
      Operation Operations.NoContent                   =.        systemOperation (ignoreConfig noContent)
      Operation Operations.NotAcceptable               =.        systemOperation (ignoreConfig notAcceptable)
      Operation Operations.NotFound                    =.        systemOperation (ignoreConfig notFound)
      Operation Operations.NotImplemented              =.        systemOperation (ignoreConfig notImplemented)
      Operation Operations.NotModified                 =.        systemOperation notModified
      Operation Operations.OK                          =.        systemOperation ok
      Operation Operations.Options                     =.        systemOperation options
      Operation Operations.PreconditionFailed          =.        systemOperation (ignoreConfig preconditionFailed)
      Operation Operations.RequestEntityTooLarge       =.        systemOperation (ignoreConfig requestEntityTooLarge)
      Operation Operations.SeeOther                    =.        systemOperation seeOther
      Operation Operations.ServiceUnavailable          =.        systemOperation (ignoreConfig serviceUnavailable)
      Operation Operations.Unauthorized                =.        systemOperation (ignoreConfig unauthorized)
      Operation Operations.UnknownMethod               =.        systemOperation (ignoreConfig unknownMethod)
      Operation Operations.UnprocessableEntity         =.        systemOperation (ignoreConfig unprocessableEntity)
      Operation Operations.UnsupportedMediaType        =.        systemOperation (ignoreConfig unsupportedMediaType)
      Operation Operations.UriTooLong                  =.        systemOperation (ignoreConfig uriTooLong)

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