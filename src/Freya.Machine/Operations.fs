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
module internal Freya.Machine.Operations

open Freya.Core
open Freya.Core.Operators
open Freya.Types.Cors
open Freya.Types.Http

(* Abbreviations *)

module Req = Request.Headers
module Res = Response.Headers

(* Operations

   Operation nodes represent some consistent action (such as setting headers
   or similar which must take place as part of the execution but does not need
   to be overridden as it will always apply. They are most commonly seen before
   Handler nodes, to make sure that correct header values are set (though the
   handler could override them). Operation nodes cannot be user overridden. *)

(* Handlers *)

[<RequireQualifiedAccess>]
module private Handlers =

    let operation statusCode reasonPhrase =
           setPLM Response.statusCode statusCode
        *> setPLM Response.reasonPhrase reasonPhrase

(* CORS *)

[<RequireQualifiedAccess>]
module private Cors =

    (* Configuration *)

    let private headersExposed =
            (function | Some x -> x
                      | _ -> [])
        <!> config Configuration.CorsHeadersExposed

    let private headersSupported =
            (function | Some x -> x
                      | _ -> [])
        <!> config Configuration.CorsHeadersSupported

    let private methodsSupported =
            (function | Some x -> x
                      | _ -> [])
        <!> config Configuration.CorsMethodsSupported

    (* Requested *)

    let private accessControlRequestHeaders =
            (function | Some (AccessControlRequestHeaders x) -> x
                      | _ -> [])
        <!> getPLM Req.accessControlRequestHeaders

    let private accessControlRequestMethod =
            (Option.map (fun (AccessControlRequestMethod x) -> x) >> Option.get)
        <!> getPLM Req.accessControlRequestMethod

    let private origin' =
            (Option.map (fun (Origin x) -> x) >> Option.get) 
        <!> getPLM Req.origin

    (* Allowed *)

    let private headersAllowed =
            (fun headers supported ->
                match List.forall (fun x -> List.exists ((=) x) supported) headers with
                | true -> headers
                | _ -> [])
        <!> accessControlRequestHeaders
        <*> headersSupported

    let private methodsAllowed =
            (fun meth supported ->
                match List.exists ((=) meth) supported with
                | true -> [ meth ]
                | _ -> [])
        <!> accessControlRequestMethod
        <*> methodsSupported

    (* Headers *)

    let private setAccessControlAllowMethods =
        setPLM Res.accessControlAllowMethods =<< (AccessControlAllowMethods <!> methodsAllowed)

    let private setAccessControlExposeHeaders =
        setPLM Res.accessControlExposeHeaders =<< (AccessControlExposeHeaders <!> headersExposed)

    let private setAccessControlAllowHeaders =
        setPLM Res.accessControlAllowHeaders =<< (AccessControlAllowHeaders <!> headersAllowed)

    let private setOrigin =
        setPLM Res.accessControlAllowOrigin =<< ((Origins >> AccessControlAllowOrigin) <!> origin')

    (* Operations *)

    let actual =
        setAccessControlExposeHeaders

    let origin =
        setOrigin

    let preflight =
        setAccessControlAllowMethods *> setAccessControlAllowHeaders


let private operationDefinitions = 
    [ Operations.SetOK,                          (Handlers.operation 200 "OK"),                          Handlers.OK
      Operations.SetOptions,                     (Handlers.operation 200 "Options"),                     Handlers.Options
      Operations.SetCreated,                     (Handlers.operation 201 "Created"),                     Handlers.Created
      Operations.SetAccepted,                    (Handlers.operation 202 "Accepted"),                    Handlers.Accepted
      Operations.SetNoContent,                   (Handlers.operation 204 "No Content"),                  Handlers.NoContent
      Operations.SetMovedPermanently,            (Handlers.operation 301 "Moved Permanently"),           Handlers.MovedPermanently
      Operations.SetSeeOther,                    (Handlers.operation 303 "See Other"),                   Handlers.SeeOther
      Operations.SetNotModified,                 (Handlers.operation 304 "Not Modified"),                Handlers.NotModified
      Operations.SetMovedTemporarily,            (Handlers.operation 307 "Moved Temporarily"),           Handlers.MovedTemporarily
      Operations.SetMultipleRepresentations,     (Handlers.operation 310 "Multiple Representations"),    Handlers.MultipleRepresentations
      Operations.SetMalformed,                   (Handlers.operation 400 "Bad Request"),                 Handlers.Malformed
      Operations.SetUnauthorized,                (Handlers.operation 401 "Unauthorized"),                Handlers.Unauthorized
      Operations.SetForbidden,                   (Handlers.operation 403 "Forbidden"),                   Handlers.Forbidden
      Operations.SetNotFound,                    (Handlers.operation 404 "Not Found"),                   Handlers.NotFound
      Operations.SetMethodNotAllowed,            (Handlers.operation 405 "Method Not Allowed"),          Handlers.MethodNotAllowed
      Operations.SetNotAcceptable,               (Handlers.operation 406 "Not Acceptable"),              Handlers.NotAcceptable
      Operations.SetConflict,                    (Handlers.operation 409 "Conflict"),                    Handlers.Conflict
      Operations.SetGone,                        (Handlers.operation 410 "Gone"),                        Handlers.Gone
      Operations.SetPreconditionFailed,          (Handlers.operation 412 "Precondition Failed"),         Handlers.PreconditionFailed
      Operations.SetRequestEntityTooLarge,       (Handlers.operation 413 "Request Entity Too Large"),    Handlers.RequestEntityTooLarge
      Operations.SetUriTooLong,                  (Handlers.operation 414 "URI Too Long"),                Handlers.UriTooLong
      Operations.SetUnsupportedMediaType,        (Handlers.operation 415 "Unsupported Media Type"),      Handlers.UnsupportedMediaType
      Operations.SetUnprocessableEntity,         (Handlers.operation 422 "Unprocessable Entity"),        Handlers.UnprocessableEntity
      Operations.SetNotImplemented,              (Handlers.operation 501 "Not Implemented"),             Handlers.NotImplemented
      Operations.SetUnknownMethod,               (Handlers.operation 501 "Unknown Method"),              Handlers.UnknownMethod
      Operations.SetServiceUnavailable,          (Handlers.operation 503 "Service Unavailable"),         Handlers.ServiceUnavailable
      
      Operations.SetCorsActual,                  Cors.actual,                                            Operations.SetCorsOrigin
      Operations.SetCorsOrigin,                  Cors.origin,                                            Decisions.MethodOptions
      Operations.SetCorsPreflight,               Cors.preflight,                                         Operations.SetCorsOrigin ] 
        
let operations =
    operationDefinitions
    |> List.map (fun (id, operation, next) -> 
            OperationNode { Id = id
                            Operation = operation
                            Next = next })
