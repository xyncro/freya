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

/// Provides basic http operations for reuse also without machine graph.
[<RequireQualifiedAccess>]
module Freya.Machine.Extensions.Http.Operations

open System
open Arachne.Http
open Freya.Core
open Freya.Core.Operators
open Freya.Lenses.Http

(* Helpers *)

let private allow =
    Allow >> Some >> (.=) Response.Headers.allow_

let private date =
    Date.Date >> Some >> (.=) Response.Headers.date_ <| DateTime.UtcNow

let private eTag =
    Option.map ETag >> (.=) Response.Headers.eTag_

let private expires =
    Option.map Expires >> (.=) Response.Headers.expires_

let private lastModified =
    Option.map LastModified >> (.=) Response.Headers.lastModified_

let private location =
    Option.map Location >> (.=) Response.Headers.location_

let private phrase =
    Some >> (.=) Response.reasonPhrase_

let private status =
    Some >> (.=) Response.statusCode_

(* Operations *)

/// Sets status code to 202, reason phrase to 'Accepted', and date to UTC now.
let accepted =
        status 202
     *> phrase "Accepted"
     *> date

/// Sets status code to 404, reason phrase to 'Bad Request', and date to UTC now.
let badRequest =
        status 400
     *> phrase "Bad Request"
     *> date

/// Sets status code to 409, reason phrase to 'Conflict', and date to UTC now.
let conflict =
        status 409
     *> phrase "Conflict"
     *> date

/// Sets status code to 201, reason phrase to 'Created', location to given uri, and date to UTC now.
let created uri =
        status 201
     *> phrase "Created"
     *> date
     *> location uri

/// Sets status code to 403, reason phrase to 'Forbidden', and date to UTC now.
let forbidden =
        status 403
     *> phrase "Forbidden"
     *> date

/// Sets status code to 410, reason phrase to 'Gone', and date to UTC now.
let gone =
        status 410 
     *> phrase "Gone"
     *> date

/// Sets status code to 405, reason phrase to 'Method Not Allowed', allow to given allowed methods, and date to UTC now.
let methodNotAllowed allowedMethods =
        status 405
     *> phrase "Method Not Allowed"
     *> allow allowedMethods
     *> date

/// Sets status code to 301, reason phrase to 'Moved Permanently', location to given uri, and date to UTC now.
let movedPermanently uri =
        status 301
     *> phrase "Moved Permanently"
     *> date
     *> location uri

/// Sets status code to 307, reason phrase to 'Moved Temporarily', location to given uri, and date to UTC now.
let movedTemporarily uri =
        status 307
     *> phrase "Moved Temporarily"
     *> date
     *> location uri

/// Sets status code to 310, reason phrase to 'Multiple Representations', and date to UTC now.
let multipleRepresentations =
        status 310
     *> phrase "Multiple Representations"
     *> date

/// Sets status code to 204, reason phrase to 'No Content', and date to UTC now.
let noContent =
        status 204
     *> phrase "No Content"
     *> date

/// Sets status code to 406, reason phrase to 'Not Acceptable', and date to UTC now.
let notAcceptable =
        status 406
     *> phrase "Not Acceptable"
     *> date

/// Sets status code to 404, reason phrase to 'Not Found', and date to UTC now.
let notFound =
        status 404
     *> phrase "Not Found"
     *> date

/// Sets status code to 501, reason phrase to 'Not Implemented', and date to UTC now.
let notImplemented =
        status 501
     *> phrase "Not Implemented"
     *> date

/// Sets status code to 304, reason phrase to 'Not Modified', lastModified to given modification date,
/// eTag to given entity tag, expires to given expiry date, and date to UTC now.
let notModified modificationDate entityTag expiryDate =
        status 304
     *> phrase "Not Modified"
     *> lastModified modificationDate
     *> date
     *> eTag entityTag
     *> expires expiryDate

/// Sets status code to 200, reason phrase to 'OK', lastModified to given modification date,
/// eTag to given entity tag, expires to given expiry date, and date to UTC now.
let ok modificationDate entityTag expiryDate =
        status 200
     *> phrase "OK"
     *> lastModified modificationDate
     *> date
     *> eTag entityTag
     *> expires expiryDate

/// Sets status code to 200, reason phrase to 'Options', lastModified to given modification date,
/// eTag to given entity tag, expires to given expiry date, and date to UTC now.
let options modificationDate entityTag expiryDate =
        status 200
     *> phrase "Options"
     *> lastModified modificationDate
     *> date
     *> eTag entityTag
     *> expires expiryDate

/// Sets status code to 412, reason phrase to 'Precondition Failed', and date to UTC now.
let preconditionFailed =
        status 412
     *> phrase "Precondition Failed"
     *> date

/// Sets status code to 413, reason phrase to 'Request Entity Too Large', and date to UTC now.
let requestEntityTooLarge =
        status 413
     *> phrase "Request Entity Too Large"
     *> date

/// Sets status code to 303, reason phrase to 'See Other', location to given uri, and date to UTC now.
let seeOther uri =
        status 303
     *> phrase "See Other"
     *> date
     *> location uri

/// Sets status code to 503, reason phrase to 'Service Unavailable', and date to UTC now.
let serviceUnavailable =
        status 503
     *> phrase "Service Unavailable"
     *> date

/// Sets status code to 401, reason phrase to 'Unauthorized', and date to UTC now.
let unauthorized =
        status 401
     *> phrase "Unauthorized"
     *> date

/// Sets status code to 501, reason phrase to 'Unknown Method', and date to UTC now.
let unknownMethod =
        status 501
     *> phrase "Unknown Method"
     *> date

/// Sets status code to 422, reason phrase to 'Unprocessable Entity', and date to UTC now.
let unprocessableEntity =
        status 422
     *> phrase "Unprocessable Entity"
     *> date

/// Sets status code to 415, reason phrase to 'UnsupportedMediaType', and date to UTC now.
let unsupportedMediaType =
        status 415
     *> phrase "UnsupportedMediaType"
     *> date

/// Sets status code to 414, reason phrase to 'URI Too Long', and date to UTC now.
let uriTooLong =
        status 414
     *> phrase "URI Too Long"
     *> date

(* System operations *)

open Freya.Machine

module internal SystemOperation =

    let private systemOperation f =
        Some (Compile (fun config ->
            Compiled (Unary (f config), unconfigurable)))

    let private getMappedOrDefault f defaultValue key =
           Configuration.get key
        >> Option.map f
        >> Option.orElse (defaultValue)

    let private getOptional key =
        getMappedOrDefault ((<!>) Some) (Freya.init None) key

    let private eTag =
        getOptional Properties.ETag

    let private expires =
        getOptional Properties.Expires

    let private lastModified =
        getOptional Properties.LastModified

    let private location =
        getOptional Properties.Location

    let private methodsSupported =
        getMappedOrDefault id Defaults.methodsSupported Properties.MethodsSupported

    let created =
            location
        >=> created
         |> systemOperation

    let methodNotAllowed =
            methodsSupported
        >=> methodNotAllowed
         |> systemOperation

    let movedPermanently =
            location
        >=> movedPermanently
         |> systemOperation

    let movedTemporarily =
            location
        >=> movedTemporarily
         |> systemOperation

    let notModified =
        (fun config ->
            notModified
        <!> lastModified config
        <*> eTag config
        <*> expires config
        >>= id)
         |> systemOperation

    let ok =
        (fun config ->
            ok
        <!> lastModified config
        <*> eTag config
        <*> expires config
        >>= id)
         |> systemOperation

    let options =
        (fun config ->
            options
        <!> lastModified config
        <*> eTag config
        <*> expires config
        >>= id)
         |> systemOperation

    let seeOther =
            location
        >=> seeOther
         |> systemOperation

    let noConfig x = systemOperation (fun _ -> x)

(* Graph *)

open Freya.Machine.Operators

let internal operations =
    [ Operation Operations.Accepted                    =.        SystemOperation.noConfig accepted
      Operation Operations.BadRequest                  =.        SystemOperation.noConfig badRequest
      Operation Operations.Conflict                    =.        SystemOperation.noConfig conflict
      Operation Operations.Created                     =.        SystemOperation.created
      Operation Operations.Forbidden                   =.        SystemOperation.noConfig forbidden
      Operation Operations.Gone                        =.        SystemOperation.noConfig gone
      Operation Operations.MethodNotAllowed            =.        SystemOperation.methodNotAllowed
      Operation Operations.MovedPermanently            =.        SystemOperation.movedPermanently
      Operation Operations.MovedTemporarily            =.        SystemOperation.movedTemporarily
      Operation Operations.MultipleRepresentations     =.        SystemOperation.noConfig multipleRepresentations
      Operation Operations.NoContent                   =.        SystemOperation.noConfig noContent
      Operation Operations.NotAcceptable               =.        SystemOperation.noConfig notAcceptable
      Operation Operations.NotFound                    =.        SystemOperation.noConfig notFound
      Operation Operations.NotImplemented              =.        SystemOperation.noConfig notImplemented
      Operation Operations.NotModified                 =.        SystemOperation.notModified
      Operation Operations.OK                          =.        SystemOperation.ok
      Operation Operations.Options                     =.        SystemOperation.options
      Operation Operations.PreconditionFailed          =.        SystemOperation.noConfig preconditionFailed
      Operation Operations.RequestEntityTooLarge       =.        SystemOperation.noConfig requestEntityTooLarge
      Operation Operations.SeeOther                    =.        SystemOperation.seeOther
      Operation Operations.ServiceUnavailable          =.        SystemOperation.noConfig serviceUnavailable
      Operation Operations.Unauthorized                =.        SystemOperation.noConfig unauthorized
      Operation Operations.UnknownMethod               =.        SystemOperation.noConfig unknownMethod
      Operation Operations.UnprocessableEntity         =.        SystemOperation.noConfig unprocessableEntity
      Operation Operations.UnsupportedMediaType        =.        SystemOperation.noConfig unsupportedMediaType
      Operation Operations.UriTooLong                  =.        SystemOperation.noConfig uriTooLong

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