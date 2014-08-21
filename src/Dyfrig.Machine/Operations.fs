namespace Dyfrig.Machine

open System
open Dyfrig
open Dyfrig.Http

[<AutoOpen>]
module Operations =

    // Actions

    type MachineBuilder with

        [<CustomOperation (Actions.Delete, MaintainsVariableSpaceUsingBind = true)>]
        member x.DoDelete (r, f) = 
            x.Set (r, actionsPLens Actions.Delete, f)

        [<CustomOperation (Actions.Patch, MaintainsVariableSpaceUsingBind = true)>]
        member x.DoPatch (r, f) = 
            x.Set (r, actionsPLens Actions.Patch, f)

        [<CustomOperation (Actions.Post, MaintainsVariableSpaceUsingBind = true)>]
        member x.DoPost (r, f) = 
            x.Set (r, actionsPLens Actions.Post, f)

        [<CustomOperation (Actions.Put, MaintainsVariableSpaceUsingBind = true)>]
        member x.DoPut (r, f) = 
            x.Set (r, actionsPLens Actions.Put, f)

    // Configuration

    type MachineBuilder with

        [<CustomOperation (Config.AllowedMethods, MaintainsVariableSpaceUsingBind = true)>]
        member x.AllowedMethods (r, methods: Method list) = 
            x.Set (r, configPLens Config.AllowedMethods, Set methods)

        [<CustomOperation (Config.AvailableCharsets, MaintainsVariableSpaceUsingBind = true)>]
        member x.AvailableCharsets (r, charsets: string list) = 
            x.Set (r, configPLens Config.AvailableCharsets, charsets)

        [<CustomOperation (Config.AvailableEncodings, MaintainsVariableSpaceUsingBind = true)>]
        member x.AvailableEncodings (r, encodings: string list) = 
            x.Set (r, configPLens Config.AvailableEncodings, encodings)

        [<CustomOperation (Config.AvailableLanguages, MaintainsVariableSpaceUsingBind = true)>]
        member x.AvailableLanguages (r, languages: string list) = 
            x.Set (r, configPLens Config.AvailableLanguages, languages)

        [<CustomOperation (Config.AvailableMediaTypes, MaintainsVariableSpaceUsingBind = true)>]
        member x.AvailableMediaTypes (r, mediaTypes: string list) =
            x.Set (r, configPLens Config.AvailableMediaTypes, mediaTypes)

        [<CustomOperation (Config.ETag, MaintainsVariableSpaceUsingBind = true)>]
        member x.ETag (r, etag: OwinMonad<string>) = 
            x.Set (r, configPLens Config.ETag, etag)

        [<CustomOperation (Config.KnownMethods, MaintainsVariableSpaceUsingBind = true)>]
        member x.KnownMethods (r, methods: Method list) = 
            x.Set (r, configPLens Config.KnownMethods, Set methods)

        [<CustomOperation (Config.Modified, MaintainsVariableSpaceUsingBind = true)>]
        member x.Modified (r, modified: OwinMonad<DateTime>) = 
            x.Set (r, configPLens Config.Modified, modified)

    // Decisions

    type MachineBuilder with

        [<CustomOperation (Decisions.Allowed, MaintainsVariableSpaceUsingBind = true)>]
        member x.Allowed (r, f) = 
            x.Set (r, decisionsPLens Decisions.Allowed, f)

        [<CustomOperation (Decisions.Authorized, MaintainsVariableSpaceUsingBind = true)>]
        member x.Authorized (r, f) = 
            x.Set (r, decisionsPLens Decisions.Authorized, f)

        [<CustomOperation (Decisions.CharsetAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.CharsetAvailable (r, f) = 
            x.Set (r, decisionsPLens Decisions.CharsetAvailable, f)

        [<CustomOperation (Decisions.CanPostToGone, MaintainsVariableSpaceUsingBind = true)>]
        member x.CanPostToGone (r, f) = 
            x.Set (r, decisionsPLens Decisions.CanPostToGone, f)

        [<CustomOperation (Decisions.CanPostToMissing, MaintainsVariableSpaceUsingBind = true)>]
        member x.CanPostToMissing (r, f) = 
            x.Set (r, decisionsPLens Decisions.CanPostToMissing, f)

        [<CustomOperation (Decisions.CanPutToMissing, MaintainsVariableSpaceUsingBind = true)>]
        member x.CanPutToMissing (r, f) = 
            x.Set (r, decisionsPLens Decisions.CanPutToMissing, f)

        [<CustomOperation (Decisions.ContentTypeKnown, MaintainsVariableSpaceUsingBind = true)>]
        member x.ContentTypeKnown (r, f) = 
            x.Set (r, decisionsPLens Decisions.ContentTypeKnown, f)

        [<CustomOperation (Decisions.ContentTypeValid, MaintainsVariableSpaceUsingBind = true)>]
        member x.ContentTypeValid (r, f) = 
            x.Set (r, decisionsPLens Decisions.ContentTypeValid, f)

        [<CustomOperation (Decisions.Conflict, MaintainsVariableSpaceUsingBind = true)>]
        member x.Conflict (r, f) = 
            x.Set (r, decisionsPLens Decisions.Conflict, f)

        [<CustomOperation (Decisions.Created, MaintainsVariableSpaceUsingBind = true)>]
        member x.Created (r, f) = 
            x.Set (r, decisionsPLens Decisions.Created, f)

        [<CustomOperation (Decisions.Deleted, MaintainsVariableSpaceUsingBind = true)>]
        member x.Deleted (r, f) = 
            x.Set (r, decisionsPLens Decisions.Deleted, f)

        [<CustomOperation (Decisions.EncodingAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.EncodingAvailable (r, f) = 
            x.Set (r, decisionsPLens Decisions.EncodingAvailable, f)

        [<CustomOperation (Decisions.ETagMatchesIf, MaintainsVariableSpaceUsingBind = true)>]
        member x.ETagMatchesIf (r, f) = 
            x.Set (r, decisionsPLens Decisions.ETagMatchesIf, f)

        [<CustomOperation (Decisions.ETagMatchesIfNone, MaintainsVariableSpaceUsingBind = true)>]
        member x.ETagMatchesIfNone (r, f) = 
            x.Set (r, decisionsPLens Decisions.ETagMatchesIfNone, f)

        [<CustomOperation (Decisions.Existed, MaintainsVariableSpaceUsingBind = true)>]
        member x.Existed (r, f) = 
            x.Set (r, decisionsPLens Decisions.Existed, f)

        [<CustomOperation (Decisions.Exists, MaintainsVariableSpaceUsingBind = true)>]
        member x.Exists (r, f) = 
            x.Set (r, decisionsPLens Decisions.Exists, f)

        [<CustomOperation (Decisions.LanguageAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.LanguageAvailable (r, f) = 
            x.Set (r, decisionsPLens Decisions.LanguageAvailable, f)

        [<CustomOperation (Decisions.MethodAllowed, MaintainsVariableSpaceUsingBind = true)>]
        member x.MethodAllowed (r, f) = 
            x.Set (r, decisionsPLens Decisions.MethodAllowed, f)

        [<CustomOperation (Decisions.MethodKnown, MaintainsVariableSpaceUsingBind = true)>]
        member x.MethodKnown (r, f) = 
            x.Set (r, decisionsPLens Decisions.MethodKnown, f)

        [<CustomOperation (Decisions.Malformed, MaintainsVariableSpaceUsingBind = true)>]
        member x.Malformed (r, f) = 
            x.Set (r, decisionsPLens Decisions.Malformed, f)

        [<CustomOperation (Decisions.MediaTypeAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.MediaTypeAvailable (r, f) = 
            x.Set (r, decisionsPLens Decisions.MediaTypeAvailable, f)

        [<CustomOperation (Decisions.MovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
        member x.MovedPermanently (r, f) = 
            x.Set (r, decisionsPLens Decisions.MovedPermanently, f)

        [<CustomOperation (Decisions.MovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
        member x.MovedTemporarily (r, f) = 
            x.Set (r, decisionsPLens Decisions.MovedTemporarily, f)

        [<CustomOperation (Decisions.MultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
        member x.MultipleRepresentations (r, f) = 
            x.Set (r, decisionsPLens Decisions.MultipleRepresentations, f)

        [<CustomOperation (Decisions.PostRedirect, MaintainsVariableSpaceUsingBind = true)>]
        member x.PostRedirect (r, f) = 
            x.Set (r, decisionsPLens Decisions.PostRedirect, f)

        [<CustomOperation (Decisions.Processable, MaintainsVariableSpaceUsingBind = true)>]
        member x.Processable (r, f) = 
            x.Set (r, decisionsPLens Decisions.Processable, f)

        [<CustomOperation (Decisions.PutToDifferentUri, MaintainsVariableSpaceUsingBind = true)>]
        member x.PutToDifferentUri (r, f) = 
            x.Set (r, decisionsPLens Decisions.PutToDifferentUri, f)

        [<CustomOperation (Decisions.RespondWithEntity, MaintainsVariableSpaceUsingBind = true)>]
        member x.RespondWithEntity (r, f) = 
            x.Set (r, decisionsPLens Decisions.RespondWithEntity, f)

        [<CustomOperation (Decisions.ServiceAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.ServiceAvailable (r, f) = 
            x.Set (r, decisionsPLens Decisions.ServiceAvailable, f)

        [<CustomOperation (Decisions.UnmodifiedSince, MaintainsVariableSpaceUsingBind = true)>]
        member x.UnmodifiedSince (r, f) = 
            x.Set (r, decisionsPLens Decisions.UnmodifiedSince, f)

        [<CustomOperation (Decisions.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
        member x.UriTooLong (r, f) = 
            x.Set (r, decisionsPLens Decisions.UriTooLong, f)

        [<CustomOperation (Decisions.ValidEntityLength, MaintainsVariableSpaceUsingBind = true)>]
        member x.ValidEntityLength (r, f) = 
            x.Set (r, decisionsPLens Decisions.ValidEntityLength, f)

    // Handlers

    type MachineBuilder with

        // 200

        [<CustomOperation (Handlers.OK, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleOk (r, f) = 
            x.Set (r, handlersPLens Handlers.OK, f)

        [<CustomOperation (Handlers.Created, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleCreated (r, f) = 
            x.Set (r, handlersPLens Handlers.Created, f)

        [<CustomOperation (Handlers.Options, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleOptions (r, f) = 
            x.Set (r, handlersPLens Handlers.Options, f)

        [<CustomOperation (Handlers.Accepted, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleAccepted (r, f) = 
            x.Set (r, handlersPLens Handlers.Accepted, f)

        [<CustomOperation (Handlers.NoContent, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNoContent (r, f) = 
            x.Set (r, handlersPLens Handlers.NoContent, f)

        // 300

        [<CustomOperation (Handlers.MovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMovedPermanently (r, f) = 
            x.Set (r, handlersPLens Handlers.MovedPermanently, f)

        [<CustomOperation (Handlers.SeeOther, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleSeeOther (r, f) = 
            x.Set (r, handlersPLens Handlers.SeeOther, f)

        [<CustomOperation (Handlers.NotModified, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNotModified (r, f) = 
            x.Set (r, handlersPLens Handlers.NotModified, f)

        [<CustomOperation (Handlers.MovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMovedTemporarily (r, f) = 
            x.Set (r, handlersPLens Handlers.MovedTemporarily, f)

        [<CustomOperation (Handlers.MultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMultipleRepresentations (r, f) = 
            x.Set (r, handlersPLens Handlers.MultipleRepresentations, f)
        
        // 400

        [<CustomOperation (Handlers.Malformed, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMalformed (r, f) = 
            x.Set (r, handlersPLens Handlers.Malformed, f)

        [<CustomOperation (Handlers.Unauthorized, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUnauthorized (r, f) = 
            x.Set (r, handlersPLens Handlers.Unauthorized, f)

        [<CustomOperation (Handlers.Forbidden, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleForbidden (r, f) = 
            x.Set (r, handlersPLens Handlers.Forbidden, f)

        [<CustomOperation (Handlers.NotFound, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNotFound (r, f) = 
            x.Set (r, handlersPLens Handlers.NotFound, f)

        [<CustomOperation (Handlers.MethodNotAllowed, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMethodNotAllowed (r, f) = 
            x.Set (r, handlersPLens Handlers.MethodNotAllowed, f)

        [<CustomOperation (Handlers.NotAcceptable, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNotAcceptable (r, f) = 
            x.Set (r, handlersPLens Handlers.NotAcceptable, f)

        [<CustomOperation (Handlers.Conflict, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleConflict (r, f) = 
            x.Set (r, handlersPLens Handlers.Conflict, f)

        [<CustomOperation (Handlers.Gone, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleGone (r, f) = 
            x.Set (r, handlersPLens Handlers.Gone, f)

        [<CustomOperation (Handlers.PreconditionFailed, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandlePreconditionFailed (r, f) = 
            x.Set (r, handlersPLens Handlers.PreconditionFailed, f)

        [<CustomOperation (Handlers.RequestEntityTooLarge, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleRequestEntityTooLarge (r, f) = 
            x.Set (r, handlersPLens Handlers.RequestEntityTooLarge, f)

        [<CustomOperation (Handlers.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUriTooLong (r, f) = 
            x.Set (r, handlersPLens Handlers.UriTooLong, f)

        [<CustomOperation (Handlers.UnsupportedMediaType, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUnsupportedMediaType (r, f) = 
            x.Set (r, handlersPLens Handlers.UnsupportedMediaType, f)

        [<CustomOperation (Handlers.UnprocessableEntity, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUnprocessableEntity (r, f) = 
            x.Set (r, handlersPLens Handlers.UnprocessableEntity, f)

        // 500

        [<CustomOperation (Handlers.Exception, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleException (r, f) = 
            x.Set (r, handlersPLens Handlers.Exception, f)

        [<CustomOperation (Handlers.NotImplemented, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNotImplemented (r, f) = 
            x.Set (r, handlersPLens Handlers.NotImplemented, f)

        [<CustomOperation (Handlers.UnknownMethod, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUnknownMethod (r, f) = 
            x.Set (r, handlersPLens Handlers.UnknownMethod, f)
    
        [<CustomOperation (Handlers.ServiceUnavailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleServiceUnavailable (r, f) = 
            x.Set (r, handlersPLens Handlers.ServiceUnavailable, f)

    // Utility

    type MachineBuilder with
    
        [<CustomOperation ("including", MaintainsVariableSpaceUsingBind = true)>]
        member x.Including (r, h) = 
            x.Combine (r, h)

//[<AutoOpen>]
//module RoutesOperations =
//
//    type FrostRoutesBuilder with
//    
//        [<CustomOperation ("resource", MaintainsVariableSpaceUsingBind = true)>]
//        member x.Resource (r, p, res) = x.Register (r, p, compileResource res)


