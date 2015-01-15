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

   We wire up all pre-Handler Operators here, and wire each Handler up to
   the finish node. We also disconnect the expected Start -> Finish edge of
   the graph which we assume to be present (as we expect to be the first
   modification to an empty graph).

   See [https://github.com/for-GET/http-decision-diagram]. *)

(* Handlers *)

[<RequireQualifiedAccess>]
module Handlers =

    let [<Literal>] private prefix = "handle"

    let [<Literal>] Accepted = prefix + "Accepted"
    let [<Literal>] BadRequest = prefix + "BadRequest"
    let [<Literal>] Conflict = prefix + "Conflict"
    let [<Literal>] Continue = prefix + "Continue"
    let [<Literal>] Created = prefix + "Created"
    let [<Literal>] ExpectationFailed = prefix + "ExpectationFailed"
    let [<Literal>] Forbidden = prefix + "Forbidden"
    let [<Literal>] Gone = prefix + "Gone"
    let [<Literal>] HeadersTooLarge = prefix + "HeadersTooLarge"
    let [<Literal>] InternalServerError = prefix + "InternalServerError"
    let [<Literal>] MethodNotAllowed = prefix + "MethodNotAllowed"
    let [<Literal>] MultipleChoices = prefix + "MultipleChoices"
    let [<Literal>] NoContent = prefix + "NoContent"
    let [<Literal>] NotAcceptable = prefix + "NotAcceptable"
    let [<Literal>] NotFound = prefix + "NotFound"
    let [<Literal>] NotImplemented = prefix + "NotImplemented"
    let [<Literal>] NotModified = prefix + "NotModified"
    let [<Literal>] Ok = prefix + "Ok"
    let [<Literal>] PayloadTooLarge = prefix + "PayloadTooLarge"
    let [<Literal>] PermanentRedirect = prefix + "PermanentRedirect"
    let [<Literal>] PreconditionFailed = prefix + "PreconditionFailed"
    let [<Literal>] SeeOther = prefix + "SeeOther"
    let [<Literal>] ServiceUnavailable = prefix + "ServiceUnavailable"
    let [<Literal>] TemporaryRedirect = prefix + "TemporaryRedirect"
    let [<Literal>] Unauthorized = prefix + "Unauthorized"
    let [<Literal>] UnsupportedMediaType = prefix + "UnsupportedMediaType"
    let [<Literal>] UriTooLong = prefix + "UriTooLong"

    let accepted =
        handler Accepted

    let badRequest =
        handler BadRequest

    let conflict =
        handler Conflict

    let cont =
        handler Continue

    let created =
        handler Created

    let expectationFailed =
        handler ExpectationFailed

    let forbidden =
        handler Forbidden

    let gone =
        handler Gone

    let headersTooLarge =
        handler HeadersTooLarge

    let internalServerError =
        handler InternalServerError

    let methodNotAllowed =
        handler MethodNotAllowed

    let multipleChoices =
        handler MultipleChoices

    let noContent =
        handler NoContent

    let notAcceptable =
        handler NotAcceptable

    let notFound =
        handler NotFound

    let notImplemented =
        handler NotImplemented

    let notModified =
        handler NotModified

    let ok =
        handler Ok

    let payloadTooLarge =
        handler PayloadTooLarge

    let permanentRedirect =
        handler PermanentRedirect

    let preconditionFailed =
        handler PreconditionFailed

    let seeOther =
        handler SeeOther

    let serviceUnavailable =
        handler ServiceUnavailable

    let temporaryRedirect =
        handler TemporaryRedirect

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

    let [<Literal>] Accepted = prefix + "Accepted"
    let [<Literal>] BadRequest = prefix + "BadRequest"
    let [<Literal>] Conflict = prefix + "Conflict"
    let [<Literal>] Continue = prefix + "Continue"
    let [<Literal>] Created = prefix + "Created"
    let [<Literal>] ExpectationFailed = prefix + "ExpectationFailed"
    let [<Literal>] Forbidden = prefix + "Forbidden"
    let [<Literal>] Gone = prefix + "Gone"
    let [<Literal>] HeadersTooLarge = prefix + "HeadersTooLarge"
    let [<Literal>] InternalServerError = prefix + "InternalServerError"
    let [<Literal>] MethodNotAllowed = prefix + "MethodNotAllowed"
    let [<Literal>] MultipleChoices = prefix + "MultipleChoices"
    let [<Literal>] NoContent = prefix + "NoContent"
    let [<Literal>] NotAcceptable = prefix + "NotAcceptable"
    let [<Literal>] NotFound = prefix + "NotFound"
    let [<Literal>] NotImplemented = prefix + "NotImplemented"
    let [<Literal>] NotModified = prefix + "NotModified"
    let [<Literal>] Ok = prefix + "Ok"
    let [<Literal>] PayloadTooLarge = prefix + "PayloadTooLarge"
    let [<Literal>] PermanentRedirect = prefix + "PermanentRedirect"
    let [<Literal>] PreconditionFailed = prefix + "PreconditionFailed"
    let [<Literal>] SeeOther = prefix + "SeeOther"
    let [<Literal>] ServiceUnavailable = prefix + "ServiceUnavailable"
    let [<Literal>] TemporaryRedirect = prefix + "TemporaryRedirect"
    let [<Literal>] Unauthorized = prefix + "Unauthorized"
    let [<Literal>] UnsupportedMediaType = prefix + "UnsupportedMediaType"
    let [<Literal>] UriTooLong = prefix + "UriTooLong"

    let accepted =
        operation (Freya.init ())

    let badRequest =
        operation (Freya.init ())

    let conflict =
        operation (Freya.init ())

    let cont =
        operation (Freya.init ())

    let created =
        operation (Freya.init ())

    let expectationFailed =
        operation (Freya.init ())

    let forbidden =
        operation (Freya.init ())

    let gone =
        operation (Freya.init ())

    let headersTooLarge =
        operation (Freya.init ())

    let internalServerError =
        operation (Freya.init ())

    let methodNotAllowed =
        operation (Freya.init ())

    let multipleChoices =
        operation (Freya.init ())

    let noContent =
        operation (Freya.init ())

    let notAcceptable =
        operation (Freya.init ())

    let notFound =
        operation (Freya.init ())

    let notImplemented =
        operation (Freya.init ())

    let notModified =
        operation (Freya.init ())

    let ok =
        operation (Freya.init ())

    let payloadTooLarge =
        operation (Freya.init ())

    let permanentRedirect =
        operation (Freya.init ())

    let preconditionFailed =
        operation (Freya.init ())

    let seeOther =
        operation (Freya.init ())

    let serviceUnavailable =
        operation (Freya.init ())

    let temporaryRedirect =
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
        [ Ref Handlers.Accepted                                 =.        Unary Handlers.accepted
          Ref Handlers.BadRequest                               =.        Unary Handlers.badRequest
          Ref Handlers.Conflict                                 =.        Unary Handlers.conflict
          Ref Handlers.Continue                                 =.        Unary Handlers.cont
          Ref Handlers.Created                                  =.        Unary Handlers.created
          Ref Handlers.ExpectationFailed                        =.        Unary Handlers.expectationFailed
          Ref Handlers.Forbidden                                =.        Unary Handlers.forbidden
          Ref Handlers.Gone                                     =.        Unary Handlers.gone
          Ref Handlers.HeadersTooLarge                          =.        Unary Handlers.headersTooLarge
          Ref Handlers.InternalServerError                      =.        Unary Handlers.internalServerError
          Ref Handlers.MethodNotAllowed                         =.        Unary Handlers.methodNotAllowed
          Ref Handlers.MultipleChoices                          =.        Unary Handlers.multipleChoices
          Ref Handlers.NoContent                                =.        Unary Handlers.noContent
          Ref Handlers.NotAcceptable                            =.        Unary Handlers.notAcceptable
          Ref Handlers.NotImplemented                           =.        Unary Handlers.notImplemented
          Ref Handlers.NotFound                                 =.        Unary Handlers.notFound
          Ref Handlers.NotModified                              =.        Unary Handlers.notModified
          Ref Handlers.Ok                                       =.        Unary Handlers.ok
          Ref Handlers.PayloadTooLarge                          =.        Unary Handlers.payloadTooLarge
          Ref Handlers.PermanentRedirect                        =.        Unary Handlers.permanentRedirect
          Ref Handlers.PreconditionFailed                       =.        Unary Handlers.preconditionFailed
          Ref Handlers.SeeOther                                 =.        Unary Handlers.seeOther
          Ref Handlers.ServiceUnavailable                       =.        Unary Handlers.serviceUnavailable
          Ref Handlers.TemporaryRedirect                        =.        Unary Handlers.temporaryRedirect
          Ref Handlers.Unauthorized                             =.        Unary Handlers.unauthorized
          Ref Handlers.UnsupportedMediaType                     =.        Unary Handlers.unsupportedMediaType
          Ref Handlers.UriTooLong                               =.        Unary Handlers.uriTooLong

          Ref Operations.Accepted                               =.        Unary Operations.accepted
          Ref Operations.BadRequest                             =.        Unary Operations.badRequest
          Ref Operations.Conflict                               =.        Unary Operations.conflict
          Ref Operations.Continue                               =.        Unary Operations.cont
          Ref Operations.Created                                =.        Unary Operations.created
          Ref Operations.ExpectationFailed                      =.        Unary Operations.expectationFailed
          Ref Operations.Forbidden                              =.        Unary Operations.forbidden
          Ref Operations.Gone                                   =.        Unary Operations.gone
          Ref Operations.HeadersTooLarge                        =.        Unary Operations.headersTooLarge
          Ref Operations.InternalServerError                    =.        Unary Operations.internalServerError
          Ref Operations.MethodNotAllowed                       =.        Unary Operations.methodNotAllowed
          Ref Operations.MultipleChoices                        =.        Unary Operations.multipleChoices
          Ref Operations.NoContent                              =.        Unary Operations.noContent
          Ref Operations.NotAcceptable                          =.        Unary Operations.notAcceptable
          Ref Operations.NotFound                               =.        Unary Operations.notFound
          Ref Operations.NotImplemented                         =.        Unary Operations.notImplemented
          Ref Operations.NotModified                            =.        Unary Operations.notModified
          Ref Operations.Ok                                     =.        Unary Operations.ok
          Ref Operations.PayloadTooLarge                        =.        Unary Operations.payloadTooLarge
          Ref Operations.PermanentRedirect                      =.        Unary Operations.permanentRedirect
          Ref Operations.PreconditionFailed                     =.        Unary Operations.preconditionFailed
          Ref Operations.SeeOther                               =.        Unary Operations.seeOther
          Ref Operations.ServiceUnavailable                     =.        Unary Operations.serviceUnavailable
          Ref Operations.TemporaryRedirect                      =.        Unary Operations.temporaryRedirect
          Ref Operations.Unauthorized                           =.        Unary Operations.unauthorized
          Ref Operations.UnsupportedMediaType                   =.        Unary Operations.unsupportedMediaType
          Ref Operations.UriTooLong                             =.        Unary Operations.uriTooLong

          Start                                                 >/        Finish

          Ref Operations.Accepted                               >.        Ref Handlers.Accepted
          Ref Operations.BadRequest                             >.        Ref Handlers.BadRequest
          Ref Operations.Conflict                               >.        Ref Handlers.Conflict
          Ref Operations.Continue                               >.        Ref Handlers.Continue
          Ref Operations.Created                                >.        Ref Handlers.Created
          Ref Operations.ExpectationFailed                      >.        Ref Handlers.ExpectationFailed
          Ref Operations.Forbidden                              >.        Ref Handlers.Forbidden
          Ref Operations.Gone                                   >.        Ref Handlers.Gone
          Ref Operations.HeadersTooLarge                        >.        Ref Handlers.HeadersTooLarge
          Ref Operations.InternalServerError                    >.        Ref Handlers.InternalServerError
          Ref Operations.MethodNotAllowed                       >.        Ref Handlers.MethodNotAllowed
          Ref Operations.MultipleChoices                        >.        Ref Handlers.MultipleChoices
          Ref Operations.NoContent                              >.        Ref Handlers.NoContent
          Ref Operations.NotAcceptable                          >.        Ref Handlers.NotAcceptable
          Ref Operations.NotFound                               >.        Ref Handlers.NotFound
          Ref Operations.NotImplemented                         >.        Ref Handlers.NotImplemented
          Ref Operations.NotModified                            >.        Ref Handlers.NotModified
          Ref Operations.Ok                                     >.        Ref Handlers.Ok
          Ref Operations.PayloadTooLarge                        >.        Ref Handlers.PayloadTooLarge
          Ref Operations.PermanentRedirect                      >.        Ref Handlers.PermanentRedirect
          Ref Operations.PreconditionFailed                     >.        Ref Handlers.PreconditionFailed
          Ref Operations.SeeOther                               >.        Ref Handlers.SeeOther
          Ref Operations.ServiceUnavailable                     >.        Ref Handlers.ServiceUnavailable
          Ref Operations.TemporaryRedirect                      >.        Ref Handlers.TemporaryRedirect
          Ref Operations.Unauthorized                           >.        Ref Handlers.Unauthorized
          Ref Operations.UnsupportedMediaType                   >.        Ref Handlers.UnsupportedMediaType
          Ref Operations.UriTooLong                             >.        Ref Handlers.UriTooLong

          Ref Handlers.Accepted                                 >.        Finish
          Ref Handlers.BadRequest                               >.        Finish
          Ref Handlers.Conflict                                 >.        Finish
          Ref Handlers.Continue                                 >.        Finish
          Ref Handlers.Created                                  >.        Finish
          Ref Handlers.ExpectationFailed                        >.        Finish
          Ref Handlers.Forbidden                                >.        Finish
          Ref Handlers.Gone                                     >.        Finish
          Ref Handlers.HeadersTooLarge                          >.        Finish
          Ref Handlers.InternalServerError                      >.        Finish
          Ref Handlers.MethodNotAllowed                         >.        Finish
          Ref Handlers.MultipleChoices                          >.        Finish
          Ref Handlers.NoContent                                >.        Finish
          Ref Handlers.NotAcceptable                            >.        Finish
          Ref Handlers.NotFound                                 >.        Finish
          Ref Handlers.NotImplemented                           >.        Finish
          Ref Handlers.NotModified                              >.        Finish
          Ref Handlers.Ok                                       >.        Finish
          Ref Handlers.PayloadTooLarge                          >.        Finish
          Ref Handlers.PermanentRedirect                        >.        Finish
          Ref Handlers.PreconditionFailed                       >.        Finish
          Ref Handlers.SeeOther                                 >.        Finish
          Ref Handlers.ServiceUnavailable                       >.        Finish
          Ref Handlers.TemporaryRedirect                        >.        Finish
          Ref Handlers.Unauthorized                             >.        Finish
          Ref Handlers.UnsupportedMediaType                     >.        Finish
          Ref Handlers.UriTooLong                               >.        Finish ]