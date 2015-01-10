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
module internal Freya.Machine.Graph

open Aether
open Freya.Core
open Freya.Core.Operators

(* Actions

   Action nodes execute some kind of "side-effecting" logical action
   (i.e. in response to a DELETE, POST, etc. method which is generally
   non-idempotent). They will generally need overriding if the resource
   is going to support the associated method. *)

let private actionDefinitions =
    [ Actions.Delete,                           Decisions.Deleted 
      Actions.Patch,                            Decisions.RespondWithEntity
      Actions.Post,                             Decisions.PostRedirect
      Actions.Put,                              Decisions.Created ]

let private actions =
    actionDefinitions
    |> List.map (fun (id, next) ->
            ActionNode { Id = id
                         Override = 
                           { Allow = true
                             Overridden = false }
                         Action = Freya.returnM ()
                         Next = next })

(* Decisions (Public)

   Decision nodes are (or should be) side effect free and represent some
   choice to be made (depending generally on the form of the request). The
   decision returns a bool, which is then used to select which node to
   invoke next.

   Public decisions may be overridden by the resource programmer
   using declarative machine monad syntax. *)

(*    Key                                        Default                                           True                                              False
      ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- *)

let private publicDecisionDefinitions =
    [ Decisions.Allowed,                         true,                                             (Decisions.ContentTypeValid,                      Operations.Forbidden)
      Decisions.Authorized,                      true,                                             (Decisions.Allowed,                               Operations.Unauthorized)
      Decisions.AllowPostToGone,                 false,                                            (Actions.Post,                                    Operations.Gone)
      Decisions.AllowPostToMissing,              true,                                             (Actions.Post,                                    Operations.NotFound)
      Decisions.AllowPutToMissing,               true,                                             (Decisions.Conflicts,                             Operations.NotImplemented)
      Decisions.CharsetsStrict,                  false,                                            (Operations.NotAcceptable,                        Decisions.EncodingRequested)
      Decisions.Conflicts,                       false,                                            (Operations.Conflict,                             Actions.Put)
      Decisions.ContentTypeKnown,                true,                                             (Decisions.EntityLengthValid,                     Operations.UnsupportedMediaType)
      Decisions.ContentTypeValid,                true,                                             (Decisions.ContentTypeKnown,                      Operations.NotImplemented)
      Decisions.Created,                         true,                                             (Operations.Created,                              Decisions.RespondWithEntity)
      Decisions.Deleted,                         true,                                             (Decisions.RespondWithEntity,                     Operations.Accepted)
      Decisions.EncodingsStrict,                 false,                                            (Operations.NotAcceptable,                        Decisions.Processable)
      Decisions.EntityLengthValid,               true,                                             (Decisions.CorsEnabled,                           Operations.RequestEntityTooLarge)
      Decisions.Existed,                         false,                                            (Decisions.MovedPermanently,                      Decisions.MethodPostToMissing)
      Decisions.Exists,                          true,                                             (Decisions.IfMatchRequested,                      Decisions.IfMatchExistsForMissing)
      Decisions.LanguagesStrict,                 false,                                            (Operations.NotAcceptable,                        Decisions.CharsetRequested)
      Decisions.Malformed,                       false,                                            (Operations.BadRequest,                           Decisions.Authorized)
      Decisions.MediaTypesStrict,                true,                                             (Operations.NotAcceptable,                        Decisions.LanguageRequested)
      Decisions.MovedPermanently,                false,                                            (Operations.MovedPermanently,                     Decisions.MovedTemporarily)
      Decisions.MovedTemporarily,                false,                                            (Operations.MovedTemporarily,                     Decisions.MethodPostToGone)
      Decisions.MultipleRepresentations,         false,                                            (Operations.MultipleRepresentations,              Operations.OK)
      Decisions.PostRedirect,                    false,                                            (Operations.SeeOther,                             Decisions.Created)
      Decisions.Processable,                     true,                                             (Decisions.Exists,                                Operations.UnprocessableEntity)
      Decisions.PutToDifferentUri,               false,                                            (Operations.MovedPermanently,                     Decisions.AllowPutToMissing)
      Decisions.RespondWithEntity,               true,                                             (Decisions.MultipleRepresentations,               Operations.NoContent)
      Decisions.ServiceAvailable,                true,                                             (Decisions.MethodKnown,                           Operations.ServiceUnavailable)
      Decisions.UriTooLong,                      false,                                            (Operations.UriTooLong,                           Decisions.MethodSupported) ]

let private publicDecisions =
    publicDecisionDefinitions
    |> List.map (fun (id, d, (t, f)) ->
            DecisionNode { Id = id
                           Override =
                             { Allow = true
                               Overridden = false }
                           Decision = Freya.returnM d
                           True = t
                           False = f })

(* Decisions (Internal)

   Decision nodes are (or should be) side effect free and represent some
   choice to be made (depending generally on the form of the request). The
   decision returns a bool, which is then used to select which node to
   invoke next.

   Internal decisions cannot be overridden. *)

// TODO: Implement ETag Matching

let private ifETagMatchesIf =
    Freya.returnM true

let private ifETagMatchesIfNone =
    Freya.returnM true

(*    Key                                        Decision                                          True                                              False
      ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- *)

let private internalDecisionDefinitions =
    [ Decisions.CharsetNegotiable,               Charset.Decision.negotiable,                      (Decisions.EncodingRequested,                     Decisions.CharsetsStrict)
      Decisions.CharsetRequested,                Charset.Decision.requested,                       (Decisions.CharsetNegotiable,                     Decisions.EncodingRequested)
      Decisions.CorsEnabled,                     CrossOrigin.Decision.enabled,                     (Decisions.CorsOrigin,                            Decisions.MethodOptions)
      Decisions.CorsOrigin,                      CrossOrigin.Decision.origin,                      (Decisions.CorsOptions,                           Decisions.MethodOptions)
      Decisions.CorsOptions,                     CrossOrigin.Decision.options,                     (Decisions.CorsPreflight,                         Operations.CrossOriginActual)
      Decisions.CorsPreflight,                   CrossOrigin.Decision.preflight,                   (Operations.CrossOriginPreflight,                 Operations.CrossOriginActual)
      Decisions.EncodingNegotiable,              Encoding.Decision.negotiable,                     (Decisions.Processable,                           Decisions.EncodingsStrict)
      Decisions.EncodingRequested,               Encoding.Decision.requested,                      (Decisions.EncodingNegotiable,                    Decisions.Processable)
      Decisions.IfMatchAny,                      IfMatch.Decision.any,                             (Decisions.IfUnmodifiedSinceRequested,            Decisions.ETagMatchesIf)
      Decisions.IfMatchExistsForMissing,         IfMatch.Decision.requested,                       (Operations.PreconditionFailed,                   Decisions.MethodPut)
      Decisions.IfMatchRequested,                IfMatch.Decision.requested,                       (Decisions.IfMatchAny,                            Decisions.IfUnmodifiedSinceRequested)
      Decisions.IfModifiedSinceModified,         IfModifiedSince.Decision.modified,                (Decisions.MethodDelete,                          Operations.NotModified)
      Decisions.IfModifiedSinceRequested,        IfModifiedSince.Decision.requested,               (Decisions.IfModifiedSinceValid,                  Decisions.MethodDelete)
      Decisions.IfModifiedSinceValid,            IfModifiedSince.Decision.valid,                   (Decisions.IfModifiedSinceModified,               Decisions.MethodDelete)
      Decisions.IfNoneMatchAny,                  IfNoneMatch.Decision.any,                         (Decisions.MethodGetOrHead,                       Decisions.ETagMatchesIfNone)
      Decisions.IfNoneMatchRequested,            IfNoneMatch.Decision.requested,                   (Decisions.IfNoneMatchAny,                        Decisions.IfModifiedSinceRequested)
      Decisions.IfUnmodifiedSinceModified,       IfUnmodifiedSince.Decision.unmodified,            (Decisions.IfNoneMatchRequested,                  Operations.PreconditionFailed)
      Decisions.IfUnmodifiedSinceRequested,      IfUnmodifiedSince.Decision.requested,             (Decisions.IfUnmodifiedSinceValid,                Decisions.IfNoneMatchRequested)
      Decisions.IfUnmodifiedSinceValid,          IfUnmodifiedSince.Decision.valid,                 (Decisions.IfUnmodifiedSinceModified,             Decisions.IfNoneMatchRequested)
      Decisions.LanguageNegotiable,              Language.Decision.negotiable,                     (Decisions.CharsetRequested,                      Decisions.LanguagesStrict)
      Decisions.LanguageRequested,               Language.Decision.requested,                      (Decisions.LanguageNegotiable,                    Decisions.CharsetRequested)
      Decisions.MediaTypeNegotiable,             MediaType.Decision.negotiable,                    (Decisions.LanguageRequested,                     Decisions.MediaTypesStrict)
      Decisions.MediaTypeRequested,              MediaType.Decision.requested,                     (Decisions.MediaTypeNegotiable,                   Decisions.LanguageRequested)
      Decisions.MethodDelete,                    Method.Decision.delete,                           (Actions.Delete,                                  Decisions.MethodPatch)
      Decisions.MethodGetOrHead,                 Method.Decision.getOrHead,                        (Operations.NotModified,                          Operations.PreconditionFailed)
      Decisions.MethodKnown,                     Method.Decision.known,                            (Decisions.UriTooLong,                            Operations.UnknownMethod)
      Decisions.MethodOptions,                   Method.Decision.options,                          (Operations.Options,                              Decisions.MediaTypeRequested)
      Decisions.MethodPatch,                     Method.Decision.patch,                            (Actions.Patch,                                   Decisions.MethodPostToExisting)
      Decisions.MethodPostToExisting,            Method.Decision.post,                             (Actions.Post,                                    Decisions.MethodPutToExisting)
      Decisions.MethodPostToGone,                Method.Decision.post,                             (Decisions.AllowPostToGone,                       Operations.Gone)
      Decisions.MethodPostToMissing,             Method.Decision.post,                             (Decisions.AllowPostToMissing,                    Operations.NotFound)
      Decisions.MethodPut,                       Method.Decision.put,                              (Decisions.PutToDifferentUri,                     Decisions.Existed)
      Decisions.MethodPutToExisting,             Method.Decision.put,                              (Decisions.Conflicts,                             Decisions.MultipleRepresentations)
      Decisions.MethodSupported,                 Method.Decision.supported,                        (Decisions.Malformed,                             Operations.MethodNotAllowed)

      Decisions.ETagMatchesIf,                  ifETagMatchesIf,                                   (Decisions.IfUnmodifiedSinceRequested,            Operations.PreconditionFailed)
      Decisions.ETagMatchesIfNone,              ifETagMatchesIfNone,                               (Decisions.MethodGetOrHead,                       Decisions.IfModifiedSinceRequested) ]

let private internalDecisions =
    internalDecisionDefinitions
    |> List.map (fun (id, decision, (t, f)) ->
            DecisionNode { Id = id
                           Override =
                             { Allow = false
                               Overridden = false }
                           Decision = decision
                           True = t
                           False = f })

let private decisions =
    publicDecisions @ internalDecisions

(* Handlers

   Handler nodes represent the function which will return some response
   to the client. They are responsible for returning data in an appropriate
   form to Freya.Machine to be sent as part of the response. They always
   represent the final node in a traversal of the execution graph,
   so do not include any form of "next" node data. *)

let private defaultHandler _ =
    Freya.returnM {
        Metadata =
            { Charset = None
              Encodings = None
              MediaType = None
              Languages = None }
        Data = Array.empty }

(*    Key
      ------------------------------------------- *)

let private handlerDefinitions =
    [ Handlers.Accepted
      Handlers.BadRequest
      Handlers.Conflict
      Handlers.Created
      Handlers.Forbidden
      Handlers.Gone
      Handlers.MethodNotAllowed
      Handlers.MovedPermanently
      Handlers.MovedTemporarily
      Handlers.MultipleRepresentations
      Handlers.NoContent
      Handlers.NotAcceptable
      Handlers.NotFound
      Handlers.NotImplemented
      Handlers.NotModified
      Handlers.OK
      Handlers.Options
      Handlers.PreconditionFailed
      Handlers.RequestEntityTooLarge
      Handlers.SeeOther
      Handlers.ServiceUnavailable
      Handlers.Unauthorized
      Handlers.UnknownMethod
      Handlers.UnprocessableEntity
      Handlers.UnsupportedMediaType
      Handlers.UriTooLong ]

let private handlers =
    handlerDefinitions 
    |> List.map (fun id ->
            HandlerNode { Id = id
                          Override =
                            { Allow = true
                              Overridden = false }
                          Handler = defaultHandler })

(* Operations

   Operation nodes represent some consistent action (such as setting headers
   or similar which must take place as part of the execution but does not need
   to be overridden as it will always apply. They are most commonly seen before
   Handler nodes, to make sure that correct header values are set (though the
   handler could override them). Operation nodes cannot be user overridden. *)

(*    Key                                        Operation                                         Next
      ---------------------------------------------------------------------------------------------------------------------------------- *)

let private operationDefinitions = 
    [ Operations.Accepted,                       Http.Operation.accepted,                          Handlers.Accepted
      Operations.Created,                        Http.Operation.created,                           Handlers.Created
      Operations.Conflict,                       Http.Operation.conflict,                          Handlers.Conflict
      Operations.CrossOriginActual,              CrossOrigin.Operation.actual,                     Operations.CrossOriginOrigin
      Operations.CrossOriginOrigin,              CrossOrigin.Operation.origin,                     Decisions.MethodOptions
      Operations.CrossOriginPreflight,           CrossOrigin.Operation.preflight,                  Operations.CrossOriginOrigin 
      Operations.Forbidden,                      Http.Operation.forbidden,                         Handlers.Forbidden
      Operations.Gone,                           Http.Operation.gone,                              Handlers.Gone
      Operations.BadRequest,                     Http.Operation.badRequest,                        Handlers.BadRequest
      Operations.MethodNotAllowed,               Http.Operation.methodNotAllowed,                  Handlers.MethodNotAllowed
      Operations.MovedPermanently,               Http.Operation.movedPermanently,                  Handlers.MovedPermanently
      Operations.MovedTemporarily,               Http.Operation.movedTemporarily,                  Handlers.MovedTemporarily
      Operations.MultipleRepresentations,        Http.Operation.multipleRepresentations,           Handlers.MultipleRepresentations
      Operations.NoContent,                      Http.Operation.noContent,                         Handlers.NoContent
      Operations.NotAcceptable,                  Http.Operation.notAcceptable,                     Handlers.NotAcceptable
      Operations.NotFound,                       Http.Operation.notFound,                          Handlers.NotFound
      Operations.NotImplemented,                 Http.Operation.notImplemented,                    Handlers.NotImplemented
      Operations.NotModified,                    Http.Operation.notModified,                       Handlers.NotModified
      Operations.OK,                             Http.Operation.ok,                                Handlers.OK
      Operations.Options,                        Http.Operation.options,                           Handlers.Options
      Operations.PreconditionFailed,             Http.Operation.preconditionFailed,                Handlers.PreconditionFailed
      Operations.RequestEntityTooLarge,          Http.Operation.requestEntityTooLarge,             Handlers.RequestEntityTooLarge
      Operations.SeeOther,                       Http.Operation.seeOther,                          Handlers.SeeOther
      Operations.ServiceUnavailable,             Http.Operation.serviceUnavailable,                Handlers.ServiceUnavailable
      Operations.Unauthorized,                   Http.Operation.unauthorized,                      Handlers.Unauthorized
      Operations.UnknownMethod,                  Http.Operation.unknownMethod,                     Handlers.UnknownMethod
      Operations.UnprocessableEntity,            Http.Operation.unprocessableEntity,               Handlers.UnprocessableEntity
      Operations.UnsupportedMediaType,           Http.Operation.unsupportedMediaType,              Handlers.UnsupportedMediaType
      Operations.UriTooLong,                     Http.Operation.uriTooLong,                        Handlers.UriTooLong ]

let private operations =
    operationDefinitions
    |> List.map (fun (id, operation, next) -> 
            OperationNode { Id = id
                            Operation = operation
                            Next = next })

(* Graph *)

let private defaultGraph =
      actions
    @ decisions
    @ handlers
    @ operations

(* Constructors *)

let private actionNode x action =
    ActionNode { x with Action = action
                        Override = { x.Override with Overridden = true } }

let private action (x: FreyaMachineActionNode) def =
    match x.Override.Allow, getPL (actionKeyPLens x.Id) def with
    | true, Some action -> x.Id, actionNode x action
    | _ -> x.Id, ActionNode x

let private decisionNode x decision =
    DecisionNode { x with Decision = decision
                          Override = { x.Override with Overridden = true } }

let private decision (x: FreyaMachineDecisionNode) def =
    match x.Override.Allow, getPL (decisionKeyPLens x.Id) def with
    | true, Some decision -> x.Id, decisionNode x decision
    | _ -> x.Id, DecisionNode x

let private handlerNode x handler =
    HandlerNode { x with Handler = handler
                         Override = { x.Override with Overridden = true } }

let private handler (x: FreyaMachineHandlerNode) def =
    match x.Override.Allow, getPL (handlerKeyPLens x.Id) def with
    | true, Some handler -> x.Id, handlerNode x handler
    | _ -> x.Id, HandlerNode x

let private operation (x: FreyaMachineOperationNode) =
    x.Id, OperationNode x

let private node def =
    function | ActionNode x -> action x def
             | DecisionNode x -> decision x def
             | HandlerNode x -> handler x def
             | OperationNode x -> operation x

let freyaMachineGraph (definition: FreyaMachineDefinition) : FreyaMachineGraph =
    List.map (node definition) defaultGraph |> Map.ofList
