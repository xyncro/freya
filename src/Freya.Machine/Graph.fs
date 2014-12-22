[<AutoOpen>]
module internal Freya.Machine.Graph

open Aether
open Freya.Core
open Freya.Core.Operators
open Freya.Types.Http

(* Actions

   Action nodes execute some kind of "side-effecting" logical action
   (i.e. in response to a DELETE, POST, etc. method which is generally
   non-idempotent). They will generally need overriding if the resource
   is going to support the associated method. *)

let private defaultAction =
    returnM ()

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
                         Action = defaultAction
                         Next = next })

(* Decisions (Public)

   Decision nodes are (or should be) side effect free and represent some
   choice to be made (depending generally on the form of the request). The
   decision returns a bool, which is then used to select which node to
   invoke next.

   Public decisions may be overridden by the resource programmer
   using declarative machine monad syntax. *)

let private defaultDecision def =
    returnM def

let private publicDecisionDefinitions =
    [ Decisions.Allowed,                        true,                                   (Decisions.ContentTypeValid,                Operations.SetForbidden)
      Decisions.Authorized,                     true,                                   (Decisions.Allowed,                         Operations.SetUnauthorized)
      Decisions.AllowPostToGone,                false,                                  (Actions.Post,                              Operations.SetGone)
      Decisions.AllowPostToMissing,             true,                                   (Actions.Post,                              Operations.SetNotFound)
      Decisions.AllowPutToMissing,              true,                                   (Decisions.Conflicts,                       Operations.SetNotImplemented)
      Decisions.CharsetsStrict,                 false,                                  (Operations.SetNotAcceptable,               Decisions.EncodingRequested)
      Decisions.Conflicts,                      false,                                  (Operations.SetConflict,                    Actions.Put)
      Decisions.ContentTypeKnown,               true,                                   (Decisions.EntityLengthValid,               Operations.SetUnsupportedMediaType)
      Decisions.ContentTypeValid,               true,                                   (Decisions.ContentTypeKnown,                Operations.SetNotImplemented)
      Decisions.Created,                        true,                                   (Operations.SetCreated,                     Decisions.RespondWithEntity)
      Decisions.Deleted,                        true,                                   (Decisions.RespondWithEntity,               Operations.SetAccepted)
      Decisions.EncodingsStrict,                false,                                  (Operations.SetNotAcceptable,               Decisions.Processable)
      Decisions.EntityLengthValid,              true,                                   (Decisions.CorsEnabled,                     Operations.SetRequestEntityTooLarge)
      Decisions.Existed,                        false,                                  (Decisions.MovedPermanently,                Decisions.MethodPostToMissing)
      Decisions.Exists,                         true,                                   (Decisions.IfMatchRequested,                Decisions.IfMatchExistsForMissing)
      Decisions.LanguagesStrict,                false,                                  (Operations.SetNotAcceptable,               Decisions.CharsetRequested)
      Decisions.Malformed,                      false,                                  (Operations.SetMalformed,                   Decisions.Authorized)
      Decisions.MediaTypesStrict,               true,                                   (Operations.SetNotAcceptable,               Decisions.LanguageRequested)
      Decisions.MovedPermanently,               false,                                  (Operations.SetMovedPermanently,            Decisions.MovedTemporarily)
      Decisions.MovedTemporarily,               false,                                  (Operations.SetMovedTemporarily,            Decisions.MethodPostToGone)
      Decisions.MultipleRepresentations,        false,                                  (Operations.SetMultipleRepresentations,     Operations.SetOK)
      Decisions.PostRedirect,                   false,                                  (Operations.SetSeeOther,                    Decisions.Created)
      Decisions.Processable,                    true,                                   (Decisions.Exists,                          Operations.SetUnprocessableEntity)
      Decisions.PutToDifferentUri,              false,                                  (Operations.SetMovedPermanently,            Decisions.AllowPutToMissing)
      Decisions.RespondWithEntity,              true,                                   (Decisions.MultipleRepresentations,         Operations.SetNoContent)
      Decisions.ServiceAvailable,               true,                                   (Decisions.MethodKnown,                     Operations.SetServiceUnavailable)
      Decisions.UriTooLong,                     false,                                  (Operations.SetUriTooLong,                  Decisions.MethodSupported) ]

let private publicDecisions =
    publicDecisionDefinitions
    |> List.map (fun (id, def, (t, f)) ->
            DecisionNode { Id = id
                           Override =
                             { Allow = true
                               Overridden = false }
                           Decision = defaultDecision def
                           True = t
                           False = f })

(* Decisions (Internal)

   Decision nodes are (or should be) side effect free and represent some
   choice to be made (depending generally on the form of the request). The
   decision returns a bool, which is then used to select which node to
   invoke next.

   Internal decisions cannot be overridden. *)

let private ifETagMatchesIf =
    returnM true // IMPLEMENT

let private ifETagMatchesIfNone =
    returnM true // IMPLEMENT

let private internalDecisionDefinitions =
    [ Decisions.CharsetNegotiable,              Charset.Decision.negotiable,            (Decisions.EncodingRequested,               Decisions.CharsetsStrict)
      Decisions.CharsetRequested,               Charset.Decision.requested,             (Decisions.CharsetNegotiable,               Decisions.EncodingRequested)
      Decisions.CorsEnabled,                    CrossOrigin.Decision.enabled,           (Decisions.CorsOrigin,                      Decisions.MethodOptions)            
      Decisions.CorsOrigin,                     CrossOrigin.Decision.origin,            (Decisions.CorsOptions,                     Decisions.MethodOptions)
      Decisions.CorsOptions,                    CrossOrigin.Decision.options,           (Decisions.CorsPreflight,                   Operations.SetCorsActual)
      Decisions.CorsPreflight,                  CrossOrigin.Decision.preflight,         (Operations.SetCorsPreflight,               Operations.SetCorsActual)
      Decisions.EncodingNegotiable,             Encoding.Decision.negotiable,           (Decisions.Processable,                     Decisions.EncodingsStrict)
      Decisions.EncodingRequested,              Encoding.Decision.requested,            (Decisions.EncodingNegotiable,              Decisions.Processable)
      Decisions.IfMatchAny,                     IfMatch.Decision.any,                   (Decisions.IfUnmodifiedSinceRequested,      Decisions.ETagMatchesIf)
      Decisions.IfMatchExistsForMissing,        IfMatch.Decision.requested,             (Operations.SetPreconditionFailed,          Decisions.MethodPut)
      Decisions.IfMatchRequested,               IfMatch.Decision.requested,             (Decisions.IfMatchAny,                      Decisions.IfUnmodifiedSinceRequested)
      Decisions.IfModifiedSinceModified,        IfModifiedSince.Decision.modified,      (Decisions.MethodDelete,                    Operations.SetNotModified)
      Decisions.IfModifiedSinceRequested,       IfModifiedSince.Decision.requested,     (Decisions.IfModifiedSinceValid,            Decisions.MethodDelete)
      Decisions.IfModifiedSinceValid,           IfModifiedSince.Decision.valid,         (Decisions.IfModifiedSinceModified,         Decisions.MethodDelete)
      Decisions.IfNoneMatchAny,                 IfNoneMatch.Decision.any,               (Decisions.MethodGetOrHead,                 Decisions.ETagMatchesIfNone)
      Decisions.IfNoneMatchRequested,           IfNoneMatch.Decision.requested,         (Decisions.IfNoneMatchAny,                  Decisions.IfModifiedSinceRequested)
      Decisions.IfUnmodifiedSinceModified,      IfUnmodifiedSince.Decision.unmodified,  (Decisions.IfNoneMatchRequested,            Operations.SetPreconditionFailed)
      Decisions.IfUnmodifiedSinceRequested,     IfUnmodifiedSince.Decision.requested,   (Decisions.IfUnmodifiedSinceValid,          Decisions.IfNoneMatchRequested)
      Decisions.IfUnmodifiedSinceValid,         IfUnmodifiedSince.Decision.valid,       (Decisions.IfUnmodifiedSinceModified,       Decisions.IfNoneMatchRequested)
      Decisions.LanguageNegotiable,             Language.Decision.negotiable,           (Decisions.CharsetRequested,                Decisions.LanguagesStrict)
      Decisions.LanguageRequested,              Language.Decision.requested,            (Decisions.LanguageNegotiable,              Decisions.CharsetRequested)
      Decisions.MediaTypeNegotiable,            MediaType.Decision.negotiable,          (Decisions.LanguageRequested,               Decisions.MediaTypesStrict)
      Decisions.MediaTypeRequested,             MediaType.Decision.requested,           (Decisions.MediaTypeNegotiable,             Decisions.LanguageRequested)
      Decisions.MethodDelete,                   Method.Decision.delete,                 (Actions.Delete,                            Decisions.MethodPatch)
      Decisions.MethodGetOrHead,                Method.Decision.getOrHead,              (Operations.SetNotModified,                 Operations.SetPreconditionFailed)
      Decisions.MethodKnown,                    Method.Decision.known,                  (Decisions.UriTooLong,                      Operations.SetUnknownMethod)
      Decisions.MethodOptions,                  Method.Decision.options,                (Operations.SetOptions,                     Decisions.MediaTypeRequested)
      Decisions.MethodPatch,                    Method.Decision.patch,                  (Actions.Patch,                             Decisions.MethodPostToExisting)
      Decisions.MethodPostToExisting,           Method.Decision.post,                   (Actions.Post,                              Decisions.MethodPutToExisting)
      Decisions.MethodPostToGone,               Method.Decision.post,                   (Decisions.AllowPostToGone,                 Operations.SetGone)
      Decisions.MethodPostToMissing,            Method.Decision.post,                   (Decisions.AllowPostToMissing,              Operations.SetNotFound)
      Decisions.MethodPut,                      Method.Decision.put,                    (Decisions.PutToDifferentUri,               Decisions.Existed)
      Decisions.MethodPutToExisting,            Method.Decision.put,                    (Decisions.Conflicts,                       Decisions.MultipleRepresentations)
      Decisions.MethodSupported,                Method.Decision.supported,              (Decisions.Malformed,                       Operations.SetMethodNotAllowed)

      Decisions.ETagMatchesIf,                  ifETagMatchesIf,                        (Decisions.IfUnmodifiedSinceRequested,      Operations.SetPreconditionFailed)
      Decisions.ETagMatchesIfNone,              ifETagMatchesIfNone,                    (Decisions.MethodGetOrHead,                 Decisions.IfModifiedSinceRequested) ]

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
    returnM { Metadata =
                { Charset = None
                  Encodings = None
                  MediaType = None
                  Languages = None }
              Data = Array.empty }

let private handlerDefinitions =
    [ Handlers.OK
      Handlers.Options
      Handlers.Created
      Handlers.Accepted
      Handlers.NoContent
      Handlers.MovedPermanently
      Handlers.SeeOther
      Handlers.NotModified
      Handlers.MovedTemporarily
      Handlers.MultipleRepresentations
      Handlers.Malformed
      Handlers.Unauthorized
      Handlers.Forbidden
      Handlers.NotFound
      Handlers.MethodNotAllowed
      Handlers.NotAcceptable
      Handlers.Conflict
      Handlers.Gone
      Handlers.PreconditionFailed
      Handlers.RequestEntityTooLarge
      Handlers.UriTooLong
      Handlers.UnsupportedMediaType
      Handlers.UnprocessableEntity
      Handlers.NotImplemented
      Handlers.UnknownMethod
      Handlers.ServiceUnavailable ]

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

// TODO: Could this live somewhere better?

let private defaultOperation statusCode reasonPhrase =
       setPLM Response.statusCode statusCode
    *> setPLM Response.reasonPhrase reasonPhrase

let private operationDefinitions = 
    [ Operations.SetOK,                         (defaultOperation 200 "OK"),                          Handlers.OK
      Operations.SetOptions,                    (defaultOperation 200 "Options"),                     Handlers.Options
      Operations.SetCreated,                    (defaultOperation 201 "Created"),                     Handlers.Created
      Operations.SetAccepted,                   (defaultOperation 202 "Accepted"),                    Handlers.Accepted
      Operations.SetNoContent,                  (defaultOperation 204 "No Content"),                  Handlers.NoContent
      Operations.SetMovedPermanently,           (defaultOperation 301 "Moved Permanently"),           Handlers.MovedPermanently
      Operations.SetSeeOther,                   (defaultOperation 303 "See Other"),                   Handlers.SeeOther
      Operations.SetNotModified,                (defaultOperation 304 "Not Modified"),                Handlers.NotModified
      Operations.SetMovedTemporarily,           (defaultOperation 307 "Moved Temporarily"),           Handlers.MovedTemporarily
      Operations.SetMultipleRepresentations,    (defaultOperation 310 "Multiple Representations"),    Handlers.MultipleRepresentations
      Operations.SetMalformed,                  (defaultOperation 400 "Bad Request"),                 Handlers.Malformed
      Operations.SetUnauthorized,               (defaultOperation 401 "Unauthorized"),                Handlers.Unauthorized
      Operations.SetForbidden,                  (defaultOperation 403 "Forbidden"),                   Handlers.Forbidden
      Operations.SetNotFound,                   (defaultOperation 404 "Not Found"),                   Handlers.NotFound
      Operations.SetMethodNotAllowed,           (defaultOperation 405 "Method Not Allowed"),          Handlers.MethodNotAllowed
      Operations.SetNotAcceptable,              (defaultOperation 406 "Not Acceptable"),              Handlers.NotAcceptable
      Operations.SetConflict,                   (defaultOperation 409 "Conflict"),                    Handlers.Conflict
      Operations.SetGone,                       (defaultOperation 410 "Gone"),                        Handlers.Gone
      Operations.SetPreconditionFailed,         (defaultOperation 412 "Precondition Failed"),         Handlers.PreconditionFailed
      Operations.SetRequestEntityTooLarge,      (defaultOperation 413 "Request Entity Too Large"),    Handlers.RequestEntityTooLarge
      Operations.SetUriTooLong,                 (defaultOperation 414 "URI Too Long"),                Handlers.UriTooLong
      Operations.SetUnsupportedMediaType,       (defaultOperation 415 "Unsupported Media Type"),      Handlers.UnsupportedMediaType
      Operations.SetUnprocessableEntity,        (defaultOperation 422 "Unprocessable Entity"),        Handlers.UnprocessableEntity
      Operations.SetNotImplemented,             (defaultOperation 501 "Not Implemented"),             Handlers.NotImplemented
      Operations.SetUnknownMethod,              (defaultOperation 501 "Unknown Method"),              Handlers.UnknownMethod
      Operations.SetServiceUnavailable,         (defaultOperation 503 "Service Unavailable"),         Handlers.ServiceUnavailable
      
      Operations.SetCorsActual,                 CrossOrigin.Operation.actual,                         Operations.SetCorsOrigin
      Operations.SetCorsOrigin,                 CrossOrigin.Operation.origin,                         Decisions.MethodOptions
      Operations.SetCorsPreflight,              CrossOrigin.Operation.preflight,                      Operations.SetCorsOrigin ] 
        
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

(* Build *)

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

let buildGraph (definition: FreyaMachineDefinition) : FreyaMachineGraph =
    List.map (node definition) defaultGraph |> Map.ofList