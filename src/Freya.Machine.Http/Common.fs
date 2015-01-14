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
module internal Freya.Machine.Http.Common

open Freya.Core
open Freya.Machine
open Freya.Machine.Operators

(* Common

   Handlers (and Operations) of the superset of all sections of an HTTP
   processing graph, according to the HTTP decision diagram (v4.0.201410),
   defined as part of the for-GET project.

   See [https://github.com/for-GET/http-decision-diagram]. *)

(* Handlers *)

[<RequireQualifiedAccess>]
module Handlers =

    let [<Literal>] private prefix = "handle"

    let [<Literal>] BadRequest = prefix + "BadRequest"
    let [<Literal>] Continue = prefix + "Continue"
    let [<Literal>] ExpectationFailed = prefix + "ExpectationFailed"
    let [<Literal>] Forbidden = prefix + "Forbidden"
    let [<Literal>] HeadersTooLarge = prefix + "HeadersTooLarge"
    let [<Literal>] InternalServerError = prefix + "InternalServerError"
    let [<Literal>] MethodNotAllowed = prefix + "MethodNotAllowed"
    let [<Literal>] NotAcceptable = prefix + "NotAcceptable"
    let [<Literal>] NotImplemented = prefix + "NotImplemented"
    let [<Literal>] Ok = prefix + "Ok"
    let [<Literal>] PayloadTooLarge = prefix + "PayloadTooLarge"
    let [<Literal>] ServiceUnavailable = prefix + "ServiceUnavailable"
    let [<Literal>] Unauthorized = prefix + "Unauthorized"
    let [<Literal>] UnsupportedMediaType = prefix + "UnsupportedMediaType"
    let [<Literal>] UriTooLong = prefix + "UriTooLong"

    let badRequest =
        handler BadRequest

    let cont =
        handler Continue

    let expectationFailed =
        handler ExpectationFailed

    let forbidden =
        handler Forbidden

    let headersTooLarge =
        handler HeadersTooLarge

    let internalServerError =
        handler InternalServerError

    let methodNotAllowed =
        handler MethodNotAllowed

    let notAcceptable =
        handler NotAcceptable

    let notImplemented =
        handler NotImplemented

    let ok =
        handler Ok

    let payloadTooLarge =
        handler PayloadTooLarge

    let serviceUnavailable =
        handler ServiceUnavailable

    let unauthorized =
        handler Unauthorized

    let unsupportedMediaType =
        handler UnsupportedMediaType

    let uriTooLong =
        handler UriTooLong

(* Operations *)

[<RequireQualifiedAccess>]
module Operations =

    let [<Literal>] private prefix = "set"

    let [<Literal>] BadRequest = prefix + "BadRequest"
    let [<Literal>] Continue = prefix + "Continue"
    let [<Literal>] ExpectationFailed = prefix + "ExpectationFailed"
    let [<Literal>] Forbidden = prefix + "Forbidden"
    let [<Literal>] HeadersTooLarge = prefix + "HeadersTooLarge"
    let [<Literal>] InternalServerError = prefix + "InternalServerError"
    let [<Literal>] MethodNotAllowed = prefix + "MethodNotAllowed"
    let [<Literal>] NotAcceptable = prefix + "NotAcceptable"
    let [<Literal>] NotImplemented = prefix + "NotImplemented"
    let [<Literal>] Ok = prefix + "Ok"
    let [<Literal>] PayloadTooLarge = prefix + "PayloadTooLarge"
    let [<Literal>] ServiceUnavailable = prefix + "ServiceUnavailable"
    let [<Literal>] Unauthorized = prefix + "Unauthorized"
    let [<Literal>] UnsupportedMediaType = prefix + "UnsupportedMediaType"
    let [<Literal>] UriTooLong = prefix + "UriTooLong"

    let badRequest =
        operation (Freya.init ())

    let cont =
        operation (Freya.init ())

    let expectationFailed =
        operation (Freya.init ())

    let forbidden =
        operation (Freya.init ())

    let headersTooLarge =
        operation (Freya.init ())

    let internalServerError =
        operation (Freya.init ())

    let methodNotAllowed =
        operation (Freya.init ())

    let notAcceptable =
        operation (Freya.init ())

    let notImplemented =
        operation (Freya.init ())

    let ok =
        operation (Freya.init ())

    let payloadTooLarge =
        operation (Freya.init ())

    let serviceUnavailable =
        operation (Freya.init ())

    let unauthorized =
        operation (Freya.init ())

    let unsupportedMediaType =
        operation (Freya.init ())

    let uriTooLong =
        operation (Freya.init ())

[<RequireQualifiedAccess>]
module Graph =

    let operations =
        [ Ref Handlers.BadRequest                          .|=       Unary Handlers.badRequest
          Ref Handlers.Continue                            .|=       Unary Handlers.cont
          Ref Handlers.ExpectationFailed                   .|=       Unary Handlers.expectationFailed
          Ref Handlers.Forbidden                           .|=       Unary Handlers.forbidden
          Ref Handlers.HeadersTooLarge                     .|=       Unary Handlers.headersTooLarge
          Ref Handlers.InternalServerError                 .|=       Unary Handlers.internalServerError
          Ref Handlers.MethodNotAllowed                    .|=       Unary Handlers.methodNotAllowed
          Ref Handlers.NotAcceptable                       .|=       Unary Handlers.notAcceptable
          Ref Handlers.NotImplemented                      .|=       Unary Handlers.notImplemented
          Ref Handlers.Ok                                  .|=       Unary Handlers.ok
          Ref Handlers.PayloadTooLarge                     .|=       Unary Handlers.payloadTooLarge
          Ref Handlers.ServiceUnavailable                  .|=       Unary Handlers.serviceUnavailable
          Ref Handlers.Unauthorized                        .|=       Unary Handlers.unauthorized
          Ref Handlers.UnsupportedMediaType                .|=       Unary Handlers.unsupportedMediaType
          Ref Handlers.UriTooLong                          .|=       Unary Handlers.uriTooLong

          Ref Operations.BadRequest                        .|=       Unary Operations.badRequest
          Ref Operations.Continue                          .|=       Unary Operations.cont
          Ref Operations.ExpectationFailed                 .|=       Unary Operations.expectationFailed
          Ref Operations.Forbidden                         .|=       Unary Operations.forbidden
          Ref Operations.HeadersTooLarge                   .|=       Unary Operations.headersTooLarge
          Ref Operations.InternalServerError               .|=       Unary Operations.internalServerError
          Ref Operations.MethodNotAllowed                  .|=       Unary Operations.methodNotAllowed
          Ref Operations.NotAcceptable                     .|=       Unary Operations.notAcceptable
          Ref Operations.NotImplemented                    .|=       Unary Operations.notImplemented
          Ref Operations.Ok                                .|=       Unary Operations.ok
          Ref Operations.PayloadTooLarge                   .|=       Unary Operations.payloadTooLarge 
          Ref Operations.ServiceUnavailable                .|=       Unary Operations.serviceUnavailable
          Ref Operations.Unauthorized                      .|=       Unary Operations.unauthorized
          Ref Operations.UnsupportedMediaType              .|=       Unary Operations.unsupportedMediaType
          Ref Operations.UriTooLong                        .|=       Unary Operations.uriTooLong

          Start                                            /*>       Finish

          Ref Operations.BadRequest                        ..>       Ref Handlers.BadRequest
          Ref Operations.Continue                          ..>       Ref Handlers.Continue
          Ref Operations.ExpectationFailed                 ..>       Ref Handlers.ExpectationFailed
          Ref Operations.Forbidden                         ..>       Ref Handlers.Forbidden
          Ref Operations.HeadersTooLarge                   ..>       Ref Handlers.HeadersTooLarge
          Ref Operations.InternalServerError               ..>       Ref Handlers.InternalServerError
          Ref Operations.MethodNotAllowed                  ..>       Ref Handlers.MethodNotAllowed
          Ref Operations.NotAcceptable                     ..>       Ref Handlers.NotAcceptable
          Ref Operations.NotImplemented                    ..>       Ref Handlers.NotImplemented
          Ref Operations.Ok                                ..>       Ref Handlers.Ok
          Ref Operations.PayloadTooLarge                   ..>       Ref Handlers.PayloadTooLarge
          Ref Operations.ServiceUnavailable                ..>       Ref Handlers.ServiceUnavailable
          Ref Operations.Unauthorized                      ..>       Ref Handlers.Unauthorized
          Ref Operations.UnsupportedMediaType              ..>       Ref Handlers.UnsupportedMediaType
          Ref Operations.UriTooLong                        ..>       Ref Handlers.UriTooLong

          Ref Handlers.BadRequest                          ..>       Finish
          Ref Handlers.Continue                            ..>       Finish
          Ref Handlers.ExpectationFailed                   ..>       Finish
          Ref Handlers.Forbidden                           ..>       Finish
          Ref Handlers.HeadersTooLarge                     ..>       Finish
          Ref Handlers.InternalServerError                 ..>       Finish
          Ref Handlers.MethodNotAllowed                    ..>       Finish
          Ref Handlers.NotAcceptable                       ..>       Finish
          Ref Handlers.NotImplemented                      ..>       Finish
          Ref Handlers.Ok                                  ..>       Finish
          Ref Handlers.PayloadTooLarge                     ..>       Finish
          Ref Handlers.ServiceUnavailable                  ..>       Finish
          Ref Handlers.Unauthorized                        ..>       Finish
          Ref Handlers.UnsupportedMediaType                ..>       Finish
          Ref Handlers.UriTooLong                          ..>       Finish ]