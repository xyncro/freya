[<AutoOpen>]
module Freya.Machine.Syntax

open System
open Freya.Core
open Freya.Typed

(* Custom Operations

   Custom syntax operators used in the FreyaMachine computation
   expression. Custom syntax operators are used heavily and are the
   configuration mechanism for configuring a machine resource. *)

type FreyaMachineBuilder with

    (* Actions *)

    [<CustomOperation (Actions.Delete, MaintainsVariableSpaceUsingBind = true)>]
    member x.DoDelete (monad, m) = 
        x.Set (monad, actionPLens Actions.Delete, m)

    [<CustomOperation (Actions.Patch, MaintainsVariableSpaceUsingBind = true)>]
    member x.DoPatch (monad, m) = 
        x.Set (monad, actionPLens Actions.Patch, m)

    [<CustomOperation (Actions.Post, MaintainsVariableSpaceUsingBind = true)>]
    member x.DoPost (monad, m) = 
        x.Set (monad, actionPLens Actions.Post, m)

    [<CustomOperation (Actions.Put, MaintainsVariableSpaceUsingBind = true)>]
    member x.DoPut (monad, m) =
        x.Set (monad, actionPLens Actions.Put, m)

    (* Configuration *)

    [<CustomOperation (Configuration.CharsetsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.CharsetsSupported (monad, charsets: Freya<Charset list>) = 
        x.Set (monad, configurationPLens Configuration.CharsetsSupported, charsets)

    [<CustomOperation (Configuration.EncodingsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.EncodingsSupported (monad, encodings: Freya<ContentCoding list>) = 
        x.Set (monad, configurationPLens Configuration.EncodingsSupported, encodings)

    [<CustomOperation (Configuration.ETag, MaintainsVariableSpaceUsingBind = true)>]
    member x.ETag (monad, etag: Freya<EntityTag>) = 
        x.Set (monad, configurationPLens Configuration.ETag, etag)

    [<CustomOperation (Configuration.LanguagesSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.LanguagesSupported (monad, languages: Freya<LanguageTag list>) = 
        x.Set (monad, configurationPLens Configuration.LanguagesSupported, languages)

    [<CustomOperation (Configuration.LastModified, MaintainsVariableSpaceUsingBind = true)>]
    member x.LastModified (monad, modified: Freya<DateTime>) = 
        x.Set (monad, configurationPLens Configuration.LastModified, modified)

    [<CustomOperation (Configuration.MediaTypesSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.MediaTypesSupported (monad, mediaTypes: Freya<MediaType list>) =
        x.Set (monad, configurationPLens Configuration.MediaTypesSupported, mediaTypes)

    [<CustomOperation (Configuration.MethodsKnown, MaintainsVariableSpaceUsingBind = true)>]
    member x.MethodsKnown (monad, methods: Freya<Method list>) = 
        x.Set (monad, configurationPLens Configuration.MethodsKnown, methods)

    [<CustomOperation (Configuration.MethodsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.MethodsSupported (monad, methods: Freya<Method list>) = 
        x.Set (monad, configurationPLens Configuration.MethodsSupported, methods)

    (* Decisions *)

    [<CustomOperation (Decisions.Allowed, MaintainsVariableSpaceUsingBind = true)>]
    member x.Allowed (monad, m) = 
        x.Set (monad, decisionPLens Decisions.Allowed, m)

    [<CustomOperation (Decisions.AllowPostToGone, MaintainsVariableSpaceUsingBind = true)>]
    member x.AllowPostToGone (monad, m) = 
        x.Set (monad, decisionPLens Decisions.AllowPostToGone, m)

    [<CustomOperation (Decisions.AllowPostToMissing, MaintainsVariableSpaceUsingBind = true)>]
    member x.AllowPostToMissing (monad, m) = 
        x.Set (monad, decisionPLens Decisions.AllowPostToMissing, m)

    [<CustomOperation (Decisions.AllowPutToMissing, MaintainsVariableSpaceUsingBind = true)>]
    member x.AllowPutToMissing (monad, m) = 
        x.Set (monad, decisionPLens Decisions.AllowPutToMissing, m)

    [<CustomOperation (Decisions.Authorized, MaintainsVariableSpaceUsingBind = true)>]
    member x.Authorized (monad, m) = 
        x.Set (monad, decisionPLens Decisions.Authorized, m)

    [<CustomOperation (Decisions.Conflicts, MaintainsVariableSpaceUsingBind = true)>]
    member x.Conflicts (monad, m) = 
        x.Set (monad, decisionPLens Decisions.Conflicts, m)

    [<CustomOperation (Decisions.ContentTypeKnown, MaintainsVariableSpaceUsingBind = true)>]
    member x.ContentTypeKnown (monad, m) = 
        x.Set (monad, decisionPLens Decisions.ContentTypeKnown, m)

    [<CustomOperation (Decisions.ContentTypeValid, MaintainsVariableSpaceUsingBind = true)>]
    member x.ContentTypeValid (monad, m) = 
        x.Set (monad, decisionPLens Decisions.ContentTypeValid, m)

    [<CustomOperation (Decisions.Created, MaintainsVariableSpaceUsingBind = true)>]
    member x.Created (monad, m) = 
        x.Set (monad, decisionPLens Decisions.Created, m)

    [<CustomOperation (Decisions.Deleted, MaintainsVariableSpaceUsingBind = true)>]
    member x.Deleted (monad, m) = 
        x.Set (monad, decisionPLens Decisions.Deleted, m)

    [<CustomOperation (Decisions.EntityLengthValid, MaintainsVariableSpaceUsingBind = true)>]
    member x.EntityLengthValid (monad, m) = 
        x.Set (monad, decisionPLens Decisions.EntityLengthValid, m)

    [<CustomOperation (Decisions.Existed, MaintainsVariableSpaceUsingBind = true)>]
    member x.Existed (monad, m) = 
        x.Set (monad, decisionPLens Decisions.Existed, m)

    [<CustomOperation (Decisions.Exists, MaintainsVariableSpaceUsingBind = true)>]
    member x.Exists (monad, m) = 
        x.Set (monad, decisionPLens Decisions.Exists, m)

    [<CustomOperation (Decisions.Malformed, MaintainsVariableSpaceUsingBind = true)>]
    member x.Malformed (monad, m) = 
        x.Set (monad, decisionPLens Decisions.Malformed, m)

    [<CustomOperation (Decisions.MovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
    member x.MovedPermanently (monad, m) = 
        x.Set (monad, decisionPLens Decisions.MovedPermanently, m)

    [<CustomOperation (Decisions.MovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
    member x.MovedTemporarily (monad, m) = 
        x.Set (monad, decisionPLens Decisions.MovedTemporarily, m)

    [<CustomOperation (Decisions.MultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
    member x.MultipleRepresentations (monad, m) = 
        x.Set (monad, decisionPLens Decisions.MultipleRepresentations, m)

    [<CustomOperation (Decisions.PostRedirect, MaintainsVariableSpaceUsingBind = true)>]
    member x.PostRedirect (monad, m) = 
        x.Set (monad, decisionPLens Decisions.PostRedirect, m)

    [<CustomOperation (Decisions.Processable, MaintainsVariableSpaceUsingBind = true)>]
    member x.Processable (monad, m) = 
        x.Set (monad, decisionPLens Decisions.Processable, m)

    [<CustomOperation (Decisions.PutToDifferentUri, MaintainsVariableSpaceUsingBind = true)>]
    member x.PutToDifferentUri (monad, m) = 
        x.Set (monad, decisionPLens Decisions.PutToDifferentUri, m)

    [<CustomOperation (Decisions.RespondWithEntity, MaintainsVariableSpaceUsingBind = true)>]
    member x.RespondWithEntity (monad, m) = 
        x.Set (monad, decisionPLens Decisions.RespondWithEntity, m)

    [<CustomOperation (Decisions.ServiceAvailable, MaintainsVariableSpaceUsingBind = true)>]
    member x.ServiceAvailable (monad, m) = 
        x.Set (monad, decisionPLens Decisions.ServiceAvailable, m)

    [<CustomOperation (Decisions.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
    member x.UriTooLong (monad, m) = 
        x.Set (monad, decisionPLens Decisions.UriTooLong, m)

    (* Handlers *)

    // 200

    [<CustomOperation (Handlers.OK, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleOk (monad, m) = 
        x.Set (monad, handlerPLens Handlers.OK, m)

    [<CustomOperation (Handlers.Options, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleOptions (monad, m) = 
        x.Set (monad, handlerPLens Handlers.Options, m)

    [<CustomOperation (Handlers.Created, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleCreated (monad, m) = 
        x.Set (monad, handlerPLens Handlers.Created, m)

    [<CustomOperation (Handlers.Accepted, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleAccepted (monad, m) = 
        x.Set (monad, handlerPLens Handlers.Accepted, m)

    [<CustomOperation (Handlers.NoContent, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleNoContent (monad, m) = 
        x.Set (monad, handlerPLens Handlers.NoContent, m)

    // 300

    [<CustomOperation (Handlers.MovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleMovedPermanently (monad, m) = 
        x.Set (monad, handlerPLens Handlers.MovedPermanently, m)

    [<CustomOperation (Handlers.SeeOther, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleSeeOther (monad, m) = 
        x.Set (monad, handlerPLens Handlers.SeeOther, m)

    [<CustomOperation (Handlers.NotModified, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleNotModified (monad, m) = 
        x.Set (monad, handlerPLens Handlers.NotModified, m)

    [<CustomOperation (Handlers.MovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleMovedTemporarily (monad, m) = 
        x.Set (monad, handlerPLens Handlers.MovedTemporarily, m)

    [<CustomOperation (Handlers.MultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleMultipleRepresentations (monad, m) = 
        x.Set (monad, handlerPLens Handlers.MultipleRepresentations, m)
        
    // 400

    [<CustomOperation (Handlers.Malformed, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleMalformed (monad, m) = 
        x.Set (monad, handlerPLens Handlers.Malformed, m)

    [<CustomOperation (Handlers.Unauthorized, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleUnauthorized (monad, m) = 
        x.Set (monad, handlerPLens Handlers.Unauthorized, m)

    [<CustomOperation (Handlers.Forbidden, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleForbidden (monad, m) = 
        x.Set (monad, handlerPLens Handlers.Forbidden, m)

    [<CustomOperation (Handlers.NotFound, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleNotFound (monad, m) = 
        x.Set (monad, handlerPLens Handlers.NotFound, m)

    [<CustomOperation (Handlers.MethodNotAllowed, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleMethodNotAllowed (monad, m) = 
        x.Set (monad, handlerPLens Handlers.MethodNotAllowed, m)

    [<CustomOperation (Handlers.NotAcceptable, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleNotAcceptable (monad, m) = 
        x.Set (monad, handlerPLens Handlers.NotAcceptable, m)

    [<CustomOperation (Handlers.Conflict, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleConflict (monad, m) = 
        x.Set (monad, handlerPLens Handlers.Conflict, m)

    [<CustomOperation (Handlers.Gone, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleGone (monad, m) = 
        x.Set (monad, handlerPLens Handlers.Gone, m)

    [<CustomOperation (Handlers.PreconditionFailed, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandlePreconditionFailed (monad, m) = 
        x.Set (monad, handlerPLens Handlers.PreconditionFailed, m)

    [<CustomOperation (Handlers.RequestEntityTooLarge, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleRequestEntityTooLarge (monad, m) = 
        x.Set (monad, handlerPLens Handlers.RequestEntityTooLarge, m)

    [<CustomOperation (Handlers.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleUriTooLong (monad, m) = 
        x.Set (monad, handlerPLens Handlers.UriTooLong, m)

    [<CustomOperation (Handlers.UnsupportedMediaType, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleUnsupportedMediaType (monad, m) = 
        x.Set (monad, handlerPLens Handlers.UnsupportedMediaType, m)

    [<CustomOperation (Handlers.UnprocessableEntity, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleUnprocessableEntity (monad, m) = 
        x.Set (monad, handlerPLens Handlers.UnprocessableEntity, m)

    // 500

    [<CustomOperation (Handlers.Exception, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleException (monad, m) = 
        x.Set (monad, handlerPLens Handlers.Exception, m)

    [<CustomOperation (Handlers.NotImplemented, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleNotImplemented (monad, m) = 
        x.Set (monad, handlerPLens Handlers.NotImplemented, m)

    [<CustomOperation (Handlers.UnknownMethod, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleUnknownMethod (monad, m) = 
        x.Set (monad, handlerPLens Handlers.UnknownMethod, m)
    
    [<CustomOperation (Handlers.ServiceUnavailable, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleServiceUnavailable (monad, m) = 
        x.Set (monad, handlerPLens Handlers.ServiceUnavailable, m)

    (* Utility *)

    [<CustomOperation ("including", MaintainsVariableSpaceUsingBind = true)>]
    member x.Including (monad, h) = 
        x.Combine (monad, h)