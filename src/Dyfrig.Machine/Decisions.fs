[<AutoOpen>]
module internal Dyfrig.Machine.Decisions

open Dyfrig.Core.Operators

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
    [ Decisions.Allowed,                        true,                           (Decisions.ContentTypeValid,                Operations.PreForbidden)
      Decisions.Authorized,                     true,                           (Decisions.Allowed,                         Operations.PreUnauthorized)
      Decisions.AllowPostToGone,                false,                          (Actions.Post,                            Operations.PreGone)
      Decisions.AllowPostToMissing,             true,                           (Actions.Post,                            Operations.PreNotFound)
      Decisions.AllowPutToMissing,              true,                           (Decisions.Conflicts,                       Operations.PreNotImplemented)
      Decisions.Conflicts,                      false,                          (Operations.PreConflict,                     Actions.Put)
      Decisions.ContentTypeKnown,               true,                           (Decisions.EntityLengthValid,               Operations.PreUnsupportedMediaType)
      Decisions.ContentTypeValid,               true,                           (Decisions.ContentTypeKnown,                Operations.PreNotImplemented)
      Decisions.Created,                        true,                           (Operations.PreCreated,                      Decisions.RespondWithEntity)
      Decisions.Deleted,                        true,                           (Decisions.RespondWithEntity,               Operations.PreAccepted)
      Decisions.EntityLengthValid,              true,                           (Decisions.MethodOptions,                   Operations.PreRequestEntityTooLarge)
      Decisions.Existed,                        false,                          (Decisions.MovedPermanently,                Decisions.MethodPostToMissing)
      Decisions.Exists,                         true,                           (Decisions.IfMatchRequested,                Decisions.IfMatchExistsForMissing)
      Decisions.Malformed,                      false,                          (Operations.PreMalformed,                    Decisions.Authorized)
      Decisions.MovedPermanently,               false,                          (Operations.PreMovedPermanently,             Decisions.MovedTemporarily)
      Decisions.MovedTemporarily,               false,                          (Operations.PreMovedTemporarily,             Decisions.MethodPostToGone)
      Decisions.MultipleRepresentations,        false,                          (Operations.PreMultipleRepresentations,      Operations.PreOK)
      Decisions.PostRedirect,                   false,                          (Operations.PreSeeOther,                     Decisions.Created)
      Decisions.Processable,                    true,                           (Decisions.Exists,                          Operations.PreUnprocessableEntity)
      Decisions.PutToDifferentUri,              false,                          (Operations.PreMovedPermanently,             Decisions.AllowPutToMissing)
      Decisions.RespondWithEntity,              true,                           (Decisions.MultipleRepresentations,         Operations.PreNoContent)
      Decisions.ServiceAvailable,               true,                           (Decisions.MethodKnown,                     Operations.PreServiceUnavailable)
      Decisions.UriTooLong,                     false,                          (Operations.PreUriTooLong,                   Decisions.MethodSupported) ]

let private publicDecisions =
    publicDecisionDefinitions
    |> List.map (fun (id, def, (t, f)) ->
            Decision { Id = id
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
    [ Decisions.CharsetNegotiable,              Charset.negotiable,             (Decisions.EncodingRequested,               Operations.PreNotAcceptable)
      Decisions.CharsetRequested,               Charset.requested,              (Decisions.CharsetNegotiable,               Decisions.EncodingRequested)
      Decisions.EncodingNegotiable,             Encoding.negotiable,            (Decisions.Processable,                     Operations.PreNotAcceptable)
      Decisions.EncodingRequested,              Encoding.requested,             (Decisions.EncodingNegotiable,              Decisions.Processable)
      Decisions.IfMatchAny,                     IfMatch.any,                    (Decisions.IfUnmodifiedSinceRequested,      Decisions.ETagMatchesIf)
      Decisions.IfMatchExistsForMissing,        IfMatch.requested,              (Operations.PrePreconditionFailed,          Decisions.MethodPut)
      Decisions.IfMatchRequested,               IfMatch.requested,              (Decisions.IfMatchAny,                      Decisions.IfUnmodifiedSinceRequested)
      Decisions.IfModifiedSinceModified,        IfModifiedSince.modified,       (Decisions.MethodDelete,                    Operations.PreNotModified)
      Decisions.IfModifiedSinceRequested,       IfModifiedSince.requested,      (Decisions.IfModifiedSinceValid,            Decisions.MethodDelete)
      Decisions.IfModifiedSinceValid,           IfModifiedSince.valid,          (Decisions.IfModifiedSinceModified,         Decisions.MethodDelete)
      Decisions.IfNoneMatchAny,                 IfNoneMatch.any,                (Decisions.MethodGetOrHead,                 Decisions.ETagMatchesIfNone)
      Decisions.IfNoneMatchRequested,           IfNoneMatch.requested,          (Decisions.IfNoneMatchAny,                  Decisions.IfModifiedSinceRequested)
      Decisions.IfUnmodifiedSinceModified,      IfUnmodifiedSince.modified,     (Decisions.IfNoneMatchRequested,            Operations.PrePreconditionFailed)
      Decisions.IfUnmodifiedSinceRequested,     IfUnmodifiedSince.requested,    (Decisions.IfUnmodifiedSinceValid,          Decisions.IfNoneMatchRequested)
      Decisions.IfUnmodifiedSinceValid,         IfUnmodifiedSince.valid,        (Decisions.IfUnmodifiedSinceModified,       Decisions.IfNoneMatchRequested)
      Decisions.LanguageNegotiable,             Language.negotiable,            (Decisions.CharsetRequested,                Operations.PreNotAcceptable)
      Decisions.LanguageRequested,              Language.requested,             (Decisions.LanguageNegotiable,              Decisions.CharsetRequested)
      Decisions.MediaTypeNegotiable,            MediaType.negotiable,           (Decisions.LanguageRequested,               Operations.PreNotAcceptable)
      Decisions.MediaTypeRequested,             MediaType.requested,            (Decisions.MediaTypeNegotiable,             Decisions.LanguageRequested)
      Decisions.MethodDelete,                   Method.delete,                  (Actions.Delete,                            Decisions.MethodPatch)
      Decisions.MethodGetOrHead,                Method.getOrHead,               (Operations.PreNotModified,                 Operations.PrePreconditionFailed)
      Decisions.MethodKnown,                    Method.known,                   (Decisions.UriTooLong,                      Operations.PreUnknownMethod)
      Decisions.MethodOptions,                  Method.options,                 (Operations.PreOptions,                     Decisions.MediaTypeRequested)
      Decisions.MethodPatch,                    Method.patch,                   (Actions.Patch,                             Decisions.MethodPostToExisting)
      Decisions.MethodPostToExisting,           Method.post,                    (Actions.Post,                              Decisions.MethodPutToExisting)
      Decisions.MethodPostToGone,               Method.post,                    (Decisions.AllowPostToGone,                 Operations.PreGone)
      Decisions.MethodPostToMissing,            Method.post,                    (Decisions.AllowPostToMissing,              Operations.PreNotFound)
      Decisions.MethodPut,                      Method.put,                     (Decisions.PutToDifferentUri,               Decisions.Existed)
      Decisions.MethodPutToExisting,            Method.put,                     (Decisions.Conflicts,                       Decisions.MultipleRepresentations)
      Decisions.MethodSupported,                Method.supported,               (Decisions.Malformed,                       Operations.PreMethodNotAllowed)





      Decisions.ETagMatchesIf,                  ifETagMatchesIf,                (Decisions.IfUnmodifiedSinceRequested,      Operations.PrePreconditionFailed)
      Decisions.ETagMatchesIfNone,              ifETagMatchesIfNone,            (Decisions.MethodGetOrHead,                 Decisions.IfModifiedSinceRequested) ]

let private internalDecisions =
    internalDecisionDefinitions
    |> List.map (fun (id, decision, (t, f)) ->
            Decision { Id = id
                       Override =
                         { Allow = false
                           Overridden = false }
                       Decision = decision
                       True = t
                       False = f })

let decisions =
    publicDecisions @ internalDecisions