[<AutoOpen>]
module internal Freya.Machine.Decisions

open Freya.Core.Operators

(* Decisions (Public)

   Decision nodes are (or should be) side effect free and represent some
   choice to be made (depending generally on the form of the request). The
   decision returns a bool, which is then used to select which node to
   invoke next.

   Public decisions may be overridden by the resource programmer
   using declarative machine monad syntax. *)

let private decision d =
    returnM d

let private publicDecisionDefinitions =
    [ Decisions.Allowed,                        true,                           (Decisions.ContentTypeValid,                Operations.SetForbidden)
      Decisions.Authorized,                     true,                           (Decisions.Allowed,                         Operations.SetUnauthorized)
      Decisions.AllowPostToGone,                false,                          (Actions.Post,                              Operations.SetGone)
      Decisions.AllowPostToMissing,             true,                           (Actions.Post,                              Operations.SetNotFound)
      Decisions.AllowPutToMissing,              true,                           (Decisions.Conflicts,                       Operations.SetNotImplemented)
      Decisions.CharsetsStrict,                 false,                          (Operations.SetNotAcceptable,               Decisions.EncodingRequested)
      Decisions.Conflicts,                      false,                          (Operations.SetConflict,                    Actions.Put)
      Decisions.ContentTypeKnown,               true,                           (Decisions.EntityLengthValid,               Operations.SetUnsupportedMediaType)
      Decisions.ContentTypeValid,               true,                           (Decisions.ContentTypeKnown,                Operations.SetNotImplemented)
      Decisions.Created,                        true,                           (Operations.SetCreated,                     Decisions.RespondWithEntity)
      Decisions.Deleted,                        true,                           (Decisions.RespondWithEntity,               Operations.SetAccepted)
      Decisions.EncodingsStrict,                false,                          (Operations.SetNotAcceptable,               Decisions.Processable)
      Decisions.EntityLengthValid,              true,                           (Decisions.CorsEnabled,                     Operations.SetRequestEntityTooLarge)
      Decisions.Existed,                        false,                          (Decisions.MovedPermanently,                Decisions.MethodPostToMissing)
      Decisions.Exists,                         true,                           (Decisions.IfMatchRequested,                Decisions.IfMatchExistsForMissing)
      Decisions.LanguagesStrict,                false,                          (Operations.SetNotAcceptable,               Decisions.CharsetRequested)
      Decisions.Malformed,                      false,                          (Operations.SetMalformed,                   Decisions.Authorized)
      Decisions.MediaTypesStrict,               true,                           (Operations.SetNotAcceptable,               Decisions.LanguageRequested)
      Decisions.MovedPermanently,               false,                          (Operations.SetMovedPermanently,            Decisions.MovedTemporarily)
      Decisions.MovedTemporarily,               false,                          (Operations.SetMovedTemporarily,            Decisions.MethodPostToGone)
      Decisions.MultipleRepresentations,        false,                          (Operations.SetMultipleRepresentations,     Operations.SetOK)
      Decisions.PostRedirect,                   false,                          (Operations.SetSeeOther,                    Decisions.Created)
      Decisions.Processable,                    true,                           (Decisions.Exists,                          Operations.SetUnprocessableEntity)
      Decisions.PutToDifferentUri,              false,                          (Operations.SetMovedPermanently,            Decisions.AllowPutToMissing)
      Decisions.RespondWithEntity,              true,                           (Decisions.MultipleRepresentations,         Operations.SetNoContent)
      Decisions.ServiceAvailable,               true,                           (Decisions.MethodKnown,                     Operations.SetServiceUnavailable)
      Decisions.UriTooLong,                     false,                          (Operations.SetUriTooLong,                  Decisions.MethodSupported) ]

let private publicDecisions =
    publicDecisionDefinitions
    |> List.map (fun (id, def, (t, f)) ->
            DecisionNode { Id = id
                           Override =
                             { Allow = true
                               Overridden = false }
                           Decision = decision def
                           True = t
                           False = f })

(* Decisions (Internal)

   Decision nodes are (or should be) side effect free and represent some
   choice to be made (depending generally on the form of the request). The
   decision returns a bool, which is then used to select which node to
   invoke next.

   Internal decisions cannot be overridden. *)

let private ifETagMatchesIf =
    decision true // IMPLEMENT

let private ifETagMatchesIfNone =
    decision true // IMPLEMENT

let private internalDecisionDefinitions =
    [ Decisions.CharsetNegotiable,              Charset.negotiable,             (Decisions.EncodingRequested,               Decisions.CharsetsStrict)
      Decisions.CharsetRequested,               Charset.requested,              (Decisions.CharsetNegotiable,               Decisions.EncodingRequested)
      Decisions.CorsEnabled,                    Cors.enabled,                   (Decisions.CorsOrigin,                      Decisions.MethodOptions)            
      Decisions.CorsOrigin,                     Cors.origin,                    (Decisions.CorsOptions,                     Decisions.MethodOptions)
      Decisions.CorsOptions,                    Cors.options,                   (Decisions.CorsPreflight,                   Operations.SetCorsActual)
      Decisions.CorsPreflight,                  Cors.preflight,                 (Operations.SetCorsPreflight,               Operations.SetCorsActual)
      Decisions.EncodingNegotiable,             Encoding.negotiable,            (Decisions.Processable,                     Decisions.EncodingsStrict)
      Decisions.EncodingRequested,              Encoding.requested,             (Decisions.EncodingNegotiable,              Decisions.Processable)
      Decisions.IfMatchAny,                     IfMatch.any,                    (Decisions.IfUnmodifiedSinceRequested,      Decisions.ETagMatchesIf)
      Decisions.IfMatchExistsForMissing,        IfMatch.requested,              (Operations.SetPreconditionFailed,          Decisions.MethodPut)
      Decisions.IfMatchRequested,               IfMatch.requested,              (Decisions.IfMatchAny,                      Decisions.IfUnmodifiedSinceRequested)
      Decisions.IfModifiedSinceModified,        IfModifiedSince.modified,       (Decisions.MethodDelete,                    Operations.SetNotModified)
      Decisions.IfModifiedSinceRequested,       IfModifiedSince.requested,      (Decisions.IfModifiedSinceValid,            Decisions.MethodDelete)
      Decisions.IfModifiedSinceValid,           IfModifiedSince.valid,          (Decisions.IfModifiedSinceModified,         Decisions.MethodDelete)
      Decisions.IfNoneMatchAny,                 IfNoneMatch.any,                (Decisions.MethodGetOrHead,                 Decisions.ETagMatchesIfNone)
      Decisions.IfNoneMatchRequested,           IfNoneMatch.requested,          (Decisions.IfNoneMatchAny,                  Decisions.IfModifiedSinceRequested)
      Decisions.IfUnmodifiedSinceModified,      IfUnmodifiedSince.modified,     (Decisions.IfNoneMatchRequested,            Operations.SetPreconditionFailed)
      Decisions.IfUnmodifiedSinceRequested,     IfUnmodifiedSince.requested,    (Decisions.IfUnmodifiedSinceValid,          Decisions.IfNoneMatchRequested)
      Decisions.IfUnmodifiedSinceValid,         IfUnmodifiedSince.valid,        (Decisions.IfUnmodifiedSinceModified,       Decisions.IfNoneMatchRequested)
      Decisions.LanguageNegotiable,             Language.negotiable,            (Decisions.CharsetRequested,                Decisions.LanguagesStrict)
      Decisions.LanguageRequested,              Language.requested,             (Decisions.LanguageNegotiable,              Decisions.CharsetRequested)
      Decisions.MediaTypeNegotiable,            MediaType.negotiable,           (Decisions.LanguageRequested,               Decisions.MediaTypesStrict)
      Decisions.MediaTypeRequested,             MediaType.requested,            (Decisions.MediaTypeNegotiable,             Decisions.LanguageRequested)
      Decisions.MethodDelete,                   Method.delete,                  (Actions.Delete,                            Decisions.MethodPatch)
      Decisions.MethodGetOrHead,                Method.getOrHead,               (Operations.SetNotModified,                 Operations.SetPreconditionFailed)
      Decisions.MethodKnown,                    Method.known,                   (Decisions.UriTooLong,                      Operations.SetUnknownMethod)
      Decisions.MethodOptions,                  Method.options,                 (Operations.SetOptions,                     Decisions.MediaTypeRequested)
      Decisions.MethodPatch,                    Method.patch,                   (Actions.Patch,                             Decisions.MethodPostToExisting)
      Decisions.MethodPostToExisting,           Method.post,                    (Actions.Post,                              Decisions.MethodPutToExisting)
      Decisions.MethodPostToGone,               Method.post,                    (Decisions.AllowPostToGone,                 Operations.SetGone)
      Decisions.MethodPostToMissing,            Method.post,                    (Decisions.AllowPostToMissing,              Operations.SetNotFound)
      Decisions.MethodPut,                      Method.put,                     (Decisions.PutToDifferentUri,               Decisions.Existed)
      Decisions.MethodPutToExisting,            Method.put,                     (Decisions.Conflicts,                       Decisions.MultipleRepresentations)
      Decisions.MethodSupported,                Method.supported,               (Decisions.Malformed,                       Operations.SetMethodNotAllowed)





      Decisions.ETagMatchesIf,                  ifETagMatchesIf,                (Decisions.IfUnmodifiedSinceRequested,      Operations.SetPreconditionFailed)
      Decisions.ETagMatchesIfNone,              ifETagMatchesIfNone,            (Decisions.MethodGetOrHead,                 Decisions.IfModifiedSinceRequested) ]

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

let decisions =
    publicDecisions @ internalDecisions
