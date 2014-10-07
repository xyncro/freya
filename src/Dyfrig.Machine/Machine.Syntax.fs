namespace Dyfrig.Machine

open System
open Dyfrig.Core
open Dyfrig.Http


module A = Actions
module C = Configuration
module D = Decisions
module H = Handlers


[<AutoOpen>]
module Syntax =

    [<AutoOpen>]
    module Actions =

        type MachineMonadBuilder with

            [<CustomOperation (A.Delete, MaintainsVariableSpaceUsingBind = true)>]
            member x.DoDelete (r, f) = 
                x.Set (r, actionPLens A.Delete, f)

            [<CustomOperation (A.Patch, MaintainsVariableSpaceUsingBind = true)>]
            member x.DoPatch (r, f) = 
                x.Set (r, actionPLens A.Patch, f)

            [<CustomOperation (A.Post, MaintainsVariableSpaceUsingBind = true)>]
            member x.DoPost (r, f) = 
                x.Set (r, actionPLens A.Post, f)

            [<CustomOperation (A.Put, MaintainsVariableSpaceUsingBind = true)>]
            member x.DoPut (r, f) = 
                x.Set (r, actionPLens A.Put, f)


    [<AutoOpen>]
    module Configuration =

        type MachineMonadBuilder with

            [<CustomOperation (C.MethodsAllowed, MaintainsVariableSpaceUsingBind = true)>]
            member x.AllowedMethods (r, methods: Method list) = 
                x.Set (r, configPLens C.MethodsAllowed, Set methods)

            [<CustomOperation (C.CharsetsAvailable, MaintainsVariableSpaceUsingBind = true)>]
            member x.AvailableCharsets (r, charsets: string list) = 
                x.Set (r, configPLens C.CharsetsAvailable, charsets)

            [<CustomOperation (C.EncodingsAvailable, MaintainsVariableSpaceUsingBind = true)>]
            member x.AvailableEncodings (r, encodings: string list) = 
                x.Set (r, configPLens C.EncodingsAvailable, encodings)

            [<CustomOperation (C.LanguagesAvailable, MaintainsVariableSpaceUsingBind = true)>]
            member x.AvailableLanguages (r, languages: string list) = 
                x.Set (r, configPLens C.LanguagesAvailable, languages)

            [<CustomOperation (C.MediaTypesAvailable, MaintainsVariableSpaceUsingBind = true)>]
            member x.AvailableMediaTypes (r, mediaTypes: (MediaType * MediaSubType) list) =
                x.Set (r, configPLens C.MediaTypesAvailable, mediaTypes)

            [<CustomOperation (C.ResourceETag, MaintainsVariableSpaceUsingBind = true)>]
            member x.ETag (r, etag: OwinMonad<string>) = 
                x.Set (r, configPLens C.ResourceETag, etag)

            [<CustomOperation (C.MethodsKnown, MaintainsVariableSpaceUsingBind = true)>]
            member x.KnownMethods (r, methods: Method list) = 
                x.Set (r, configPLens C.MethodsKnown, Set methods)

            [<CustomOperation (C.ResourceLastModified, MaintainsVariableSpaceUsingBind = true)>]
            member x.Modified (r, modified: OwinMonad<DateTime>) = 
                x.Set (r, configPLens C.ResourceLastModified, modified)


    [<AutoOpen>]
    module Decisons =

        type MachineMonadBuilder with

            [<CustomOperation (D.RequestAllowed, MaintainsVariableSpaceUsingBind = true)>]
            member x.Allowed (r, f) = 
                x.Set (r, decisionPLens D.RequestAllowed, f)

            [<CustomOperation (D.RequestAuthorized, MaintainsVariableSpaceUsingBind = true)>]
            member x.Authorized (r, f) = 
                x.Set (r, decisionPLens D.RequestAuthorized, f)

            [<CustomOperation (D.CharsetNegotiable, MaintainsVariableSpaceUsingBind = true)>]
            member x.CharsetAvailable (r, f) = 
                x.Set (r, decisionPLens D.CharsetNegotiable, f)

            [<CustomOperation (D.CanPostToGone, MaintainsVariableSpaceUsingBind = true)>]
            member x.CanPostToGone (r, f) = 
                x.Set (r, decisionPLens D.CanPostToGone, f)

            [<CustomOperation (D.CanPostToMissing, MaintainsVariableSpaceUsingBind = true)>]
            member x.CanPostToMissing (r, f) = 
                x.Set (r, decisionPLens D.CanPostToMissing, f)

            [<CustomOperation (D.CanPutToMissing, MaintainsVariableSpaceUsingBind = true)>]
            member x.CanPutToMissing (r, f) = 
                x.Set (r, decisionPLens D.CanPutToMissing, f)

            [<CustomOperation (D.RequestContentTypeKnown, MaintainsVariableSpaceUsingBind = true)>]
            member x.ContentTypeKnown (r, f) = 
                x.Set (r, decisionPLens D.RequestContentTypeKnown, f)

            [<CustomOperation (D.RequestContentTypeValid, MaintainsVariableSpaceUsingBind = true)>]
            member x.ContentTypeValid (r, f) = 
                x.Set (r, decisionPLens D.RequestContentTypeValid, f)

            [<CustomOperation (D.ResourceConflicts, MaintainsVariableSpaceUsingBind = true)>]
            member x.Conflict (r, f) = 
                x.Set (r, decisionPLens D.ResourceConflicts, f)

            [<CustomOperation (D.ResourceCreated, MaintainsVariableSpaceUsingBind = true)>]
            member x.Created (r, f) = 
                x.Set (r, decisionPLens D.ResourceCreated, f)

            [<CustomOperation (D.ResourceDeleted, MaintainsVariableSpaceUsingBind = true)>]
            member x.Deleted (r, f) = 
                x.Set (r, decisionPLens D.ResourceDeleted, f)

            [<CustomOperation (D.EncodingNegotiable, MaintainsVariableSpaceUsingBind = true)>]
            member x.EncodingAvailable (r, f) = 
                x.Set (r, decisionPLens D.EncodingNegotiable, f)

            [<CustomOperation (D.ResourceETagMatchesIf, MaintainsVariableSpaceUsingBind = true)>]
            member x.ETagMatchesIf (r, f) = 
                x.Set (r, decisionPLens D.ResourceETagMatchesIf, f)

            [<CustomOperation (D.ResourceETagMatchesIfNone, MaintainsVariableSpaceUsingBind = true)>]
            member x.ETagMatchesIfNone (r, f) = 
                x.Set (r, decisionPLens D.ResourceETagMatchesIfNone, f)

            [<CustomOperation (D.ResourceExisted, MaintainsVariableSpaceUsingBind = true)>]
            member x.Existed (r, f) = 
                x.Set (r, decisionPLens D.ResourceExisted, f)

            [<CustomOperation (D.ResourceExists, MaintainsVariableSpaceUsingBind = true)>]
            member x.Exists (r, f) = 
                x.Set (r, decisionPLens D.ResourceExists, f)

            [<CustomOperation (D.LanguageNegotiable, MaintainsVariableSpaceUsingBind = true)>]
            member x.LanguageAvailable (r, f) = 
                x.Set (r, decisionPLens D.LanguageNegotiable, f)

            [<CustomOperation (D.MethodAllowed, MaintainsVariableSpaceUsingBind = true)>]
            member x.MethodAllowed (r, f) = 
                x.Set (r, decisionPLens D.MethodAllowed, f)

            [<CustomOperation (D.MethodKnown, MaintainsVariableSpaceUsingBind = true)>]
            member x.MethodKnown (r, f) = 
                x.Set (r, decisionPLens D.MethodKnown, f)

            [<CustomOperation (D.RequestMalformed, MaintainsVariableSpaceUsingBind = true)>]
            member x.Malformed (r, f) = 
                x.Set (r, decisionPLens D.RequestMalformed, f)

            [<CustomOperation (D.MediaTypeNegotiable, MaintainsVariableSpaceUsingBind = true)>]
            member x.MediaTypeAvailable (r, f) = 
                x.Set (r, decisionPLens D.MediaTypeNegotiable, f)

            [<CustomOperation (D.ResourceMovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
            member x.MovedPermanently (r, f) = 
                x.Set (r, decisionPLens D.ResourceMovedPermanently, f)

            [<CustomOperation (D.ResourceMovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
            member x.MovedTemporarily (r, f) = 
                x.Set (r, decisionPLens D.ResourceMovedTemporarily, f)

            [<CustomOperation (D.ResourceHasMultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
            member x.MultipleRepresentations (r, f) = 
                x.Set (r, decisionPLens D.ResourceHasMultipleRepresentations, f)

            [<CustomOperation (D.PostRedirect, MaintainsVariableSpaceUsingBind = true)>]
            member x.PostRedirect (r, f) = 
                x.Set (r, decisionPLens D.PostRedirect, f)

            [<CustomOperation (D.RequestProcessable, MaintainsVariableSpaceUsingBind = true)>]
            member x.Processable (r, f) = 
                x.Set (r, decisionPLens D.RequestProcessable, f)

            [<CustomOperation (D.PutToDifferentUri, MaintainsVariableSpaceUsingBind = true)>]
            member x.PutToDifferentUri (r, f) = 
                x.Set (r, decisionPLens D.PutToDifferentUri, f)

            [<CustomOperation (D.RespondWithEntity, MaintainsVariableSpaceUsingBind = true)>]
            member x.RespondWithEntity (r, f) = 
                x.Set (r, decisionPLens D.RespondWithEntity, f)

            [<CustomOperation (D.ServiceAvailable, MaintainsVariableSpaceUsingBind = true)>]
            member x.ServiceAvailable (r, f) = 
                x.Set (r, decisionPLens D.ServiceAvailable, f)

            [<CustomOperation (D.ResourceUnmodifiedSince, MaintainsVariableSpaceUsingBind = true)>]
            member x.UnmodifiedSince (r, f) = 
                x.Set (r, decisionPLens D.ResourceUnmodifiedSince, f)

            [<CustomOperation (D.RequestUriTooLong, MaintainsVariableSpaceUsingBind = true)>]
            member x.UriTooLong (r, f) = 
                x.Set (r, decisionPLens D.RequestUriTooLong, f)

            [<CustomOperation (D.RequestEntityLengthValid, MaintainsVariableSpaceUsingBind = true)>]
            member x.ValidEntityLength (r, f) = 
                x.Set (r, decisionPLens D.RequestEntityLengthValid, f)

    
    [<AutoOpen>]
    module Handlers =

        type MachineMonadBuilder with

            // 200

            [<CustomOperation (H.OK, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleOk (r, f) = 
                x.Set (r, handlerPLens H.OK, f)

            [<CustomOperation (H.Options, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleOptions (r, f) = 
                x.Set (r, handlerPLens H.Options, f)

            [<CustomOperation (H.Created, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleCreated (r, f) = 
                x.Set (r, handlerPLens H.Created, f)

            [<CustomOperation (H.Accepted, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleAccepted (r, f) = 
                x.Set (r, handlerPLens H.Accepted, f)

            [<CustomOperation (H.NoContent, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleNoContent (r, f) = 
                x.Set (r, handlerPLens H.NoContent, f)

            // 300

            [<CustomOperation (H.MovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleMovedPermanently (r, f) = 
                x.Set (r, handlerPLens H.MovedPermanently, f)

            [<CustomOperation (H.SeeOther, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleSeeOther (r, f) = 
                x.Set (r, handlerPLens H.SeeOther, f)

            [<CustomOperation (H.NotModified, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleNotModified (r, f) = 
                x.Set (r, handlerPLens H.NotModified, f)

            [<CustomOperation (H.MovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleMovedTemporarily (r, f) = 
                x.Set (r, handlerPLens H.MovedTemporarily, f)

            [<CustomOperation (H.MultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleMultipleRepresentations (r, f) = 
                x.Set (r, handlerPLens H.MultipleRepresentations, f)
        
            // 400

            [<CustomOperation (H.Malformed, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleMalformed (r, f) = 
                x.Set (r, handlerPLens H.Malformed, f)

            [<CustomOperation (H.Unauthorized, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleUnauthorized (r, f) = 
                x.Set (r, handlerPLens H.Unauthorized, f)

            [<CustomOperation (H.Forbidden, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleForbidden (r, f) = 
                x.Set (r, handlerPLens H.Forbidden, f)

            [<CustomOperation (H.NotFound, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleNotFound (r, f) = 
                x.Set (r, handlerPLens H.NotFound, f)

            [<CustomOperation (H.MethodNotAllowed, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleMethodNotAllowed (r, f) = 
                x.Set (r, handlerPLens H.MethodNotAllowed, f)

            [<CustomOperation (H.NotAcceptable, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleNotAcceptable (r, f) = 
                x.Set (r, handlerPLens H.NotAcceptable, f)

            [<CustomOperation (H.Conflict, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleConflict (r, f) = 
                x.Set (r, handlerPLens H.Conflict, f)

            [<CustomOperation (H.Gone, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleGone (r, f) = 
                x.Set (r, handlerPLens H.Gone, f)

            [<CustomOperation (H.PreconditionFailed, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandlePreconditionFailed (r, f) = 
                x.Set (r, handlerPLens H.PreconditionFailed, f)

            [<CustomOperation (H.RequestEntityTooLarge, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleRequestEntityTooLarge (r, f) = 
                x.Set (r, handlerPLens H.RequestEntityTooLarge, f)

            [<CustomOperation (H.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleUriTooLong (r, f) = 
                x.Set (r, handlerPLens H.UriTooLong, f)

            [<CustomOperation (H.UnsupportedMediaType, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleUnsupportedMediaType (r, f) = 
                x.Set (r, handlerPLens H.UnsupportedMediaType, f)

            [<CustomOperation (H.UnprocessableEntity, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleUnprocessableEntity (r, f) = 
                x.Set (r, handlerPLens H.UnprocessableEntity, f)

            // 500

            [<CustomOperation (H.Exception, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleException (r, f) = 
                x.Set (r, handlerPLens H.Exception, f)

            [<CustomOperation (H.NotImplemented, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleNotImplemented (r, f) = 
                x.Set (r, handlerPLens H.NotImplemented, f)

            [<CustomOperation (H.UnknownMethod, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleUnknownMethod (r, f) = 
                x.Set (r, handlerPLens H.UnknownMethod, f)
    
            [<CustomOperation (H.ServiceUnavailable, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleServiceUnavailable (r, f) = 
                x.Set (r, handlerPLens H.ServiceUnavailable, f)


    [<AutoOpen>]
    module Utility =

        type MachineMonadBuilder with
    
            [<CustomOperation ("including", MaintainsVariableSpaceUsingBind = true)>]
            member x.Including (r, h) = 
                x.Combine (r, h)
