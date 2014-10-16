namespace Dyfrig.Machine

open System
open System.Globalization
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
            member x.DoDelete (monad, m) = 
                x.Set (monad, actionPLens A.Delete, m)

            [<CustomOperation (A.Patch, MaintainsVariableSpaceUsingBind = true)>]
            member x.DoPatch (monad, m) = 
                x.Set (monad, actionPLens A.Patch, m)

            [<CustomOperation (A.Post, MaintainsVariableSpaceUsingBind = true)>]
            member x.DoPost (monad, m) = 
                x.Set (monad, actionPLens A.Post, m)

            [<CustomOperation (A.Put, MaintainsVariableSpaceUsingBind = true)>]
            member x.DoPut (monad, m) = 
                x.Set (monad, actionPLens A.Put, m)


    [<AutoOpen>]
    module Configuration =

        type MachineMonadBuilder with

            [<CustomOperation (C.CharsetsSupported, MaintainsVariableSpaceUsingBind = true)>]
            member x.CharsetsSupported (monad, charsets: SpecifiedCharset list) = 
                x.Set (monad, configurationPLens C.CharsetsSupported, charsets)

            [<CustomOperation (C.EncodingsSupported, MaintainsVariableSpaceUsingBind = true)>]
            member x.EncodingsSupported (monad, encodings: SpecifiedEncoding list) = 
                x.Set (monad, configurationPLens C.EncodingsSupported, encodings)

            [<CustomOperation (C.ETag, MaintainsVariableSpaceUsingBind = true)>]
            member x.ETag (monad, etag: OwinMonad<EntityTag>) = 
                x.Set (monad, configurationPLens C.ETag, etag)

            [<CustomOperation (C.LanguagesSupported, MaintainsVariableSpaceUsingBind = true)>]
            member x.LanguagesSupported (monad, languages: CultureInfo list) = 
                x.Set (monad, configurationPLens C.LanguagesSupported, languages)

            [<CustomOperation (C.LastModified, MaintainsVariableSpaceUsingBind = true)>]
            member x.LastModified (monad, modified: OwinMonad<DateTime>) = 
                x.Set (monad, configurationPLens C.LastModified, modified)

            [<CustomOperation (C.MediaTypesSupported, MaintainsVariableSpaceUsingBind = true)>]
            member x.MediaTypesSupported (monad, mediaTypes: ClosedMediaRange list) =
                x.Set (monad, configurationPLens C.MediaTypesSupported, mediaTypes)

            [<CustomOperation (C.MethodsKnown, MaintainsVariableSpaceUsingBind = true)>]
            member x.MethodsKnown (monad, methods: Method list) = 
                x.Set (monad, configurationPLens C.MethodsKnown, Set methods)

            [<CustomOperation (C.MethodsSupported, MaintainsVariableSpaceUsingBind = true)>]
            member x.MethodsSupported (monad, methods: Method list) = 
                x.Set (monad, configurationPLens C.MethodsSupported, Set methods)


    [<AutoOpen>]
    module Decisons =

        type MachineMonadBuilder with

            [<CustomOperation (D.Allowed, MaintainsVariableSpaceUsingBind = true)>]
            member x.Allowed (monad, m) = 
                x.Set (monad, decisionPLens D.Allowed, m)

            [<CustomOperation (D.AllowPostToGone, MaintainsVariableSpaceUsingBind = true)>]
            member x.AllowPostToGone (monad, m) = 
                x.Set (monad, decisionPLens D.AllowPostToGone, m)

            [<CustomOperation (D.AllowPostToMissing, MaintainsVariableSpaceUsingBind = true)>]
            member x.AllowPostToMissing (monad, m) = 
                x.Set (monad, decisionPLens D.AllowPostToMissing, m)

            [<CustomOperation (D.AllowPutToMissing, MaintainsVariableSpaceUsingBind = true)>]
            member x.AllowPutToMissing (monad, m) = 
                x.Set (monad, decisionPLens D.AllowPutToMissing, m)

            [<CustomOperation (D.Authorized, MaintainsVariableSpaceUsingBind = true)>]
            member x.Authorized (monad, m) = 
                x.Set (monad, decisionPLens D.Authorized, m)

            [<CustomOperation (D.Conflicts, MaintainsVariableSpaceUsingBind = true)>]
            member x.Conflicts (monad, m) = 
                x.Set (monad, decisionPLens D.Conflicts, m)

            [<CustomOperation (D.ContentTypeKnown, MaintainsVariableSpaceUsingBind = true)>]
            member x.ContentTypeKnown (monad, m) = 
                x.Set (monad, decisionPLens D.ContentTypeKnown, m)

            [<CustomOperation (D.ContentTypeValid, MaintainsVariableSpaceUsingBind = true)>]
            member x.ContentTypeValid (monad, m) = 
                x.Set (monad, decisionPLens D.ContentTypeValid, m)

            [<CustomOperation (D.Created, MaintainsVariableSpaceUsingBind = true)>]
            member x.Created (monad, m) = 
                x.Set (monad, decisionPLens D.Created, m)

            [<CustomOperation (D.Deleted, MaintainsVariableSpaceUsingBind = true)>]
            member x.Deleted (monad, m) = 
                x.Set (monad, decisionPLens D.Deleted, m)

            [<CustomOperation (D.EntityLengthValid, MaintainsVariableSpaceUsingBind = true)>]
            member x.EntityLengthValid (monad, m) = 
                x.Set (monad, decisionPLens D.EntityLengthValid, m)

            [<CustomOperation (D.Existed, MaintainsVariableSpaceUsingBind = true)>]
            member x.Existed (monad, m) = 
                x.Set (monad, decisionPLens D.Existed, m)

            [<CustomOperation (D.Exists, MaintainsVariableSpaceUsingBind = true)>]
            member x.Exists (monad, m) = 
                x.Set (monad, decisionPLens D.Exists, m)

            [<CustomOperation (D.Malformed, MaintainsVariableSpaceUsingBind = true)>]
            member x.Malformed (monad, m) = 
                x.Set (monad, decisionPLens D.Malformed, m)

            [<CustomOperation (D.MovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
            member x.MovedPermanently (monad, m) = 
                x.Set (monad, decisionPLens D.MovedPermanently, m)

            [<CustomOperation (D.MovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
            member x.MovedTemporarily (monad, m) = 
                x.Set (monad, decisionPLens D.MovedTemporarily, m)

            [<CustomOperation (D.MultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
            member x.MultipleRepresentations (monad, m) = 
                x.Set (monad, decisionPLens D.MultipleRepresentations, m)

            [<CustomOperation (D.PostRedirect, MaintainsVariableSpaceUsingBind = true)>]
            member x.PostRedirect (monad, m) = 
                x.Set (monad, decisionPLens D.PostRedirect, m)

            [<CustomOperation (D.Processable, MaintainsVariableSpaceUsingBind = true)>]
            member x.Processable (monad, m) = 
                x.Set (monad, decisionPLens D.Processable, m)

            [<CustomOperation (D.PutToDifferentUri, MaintainsVariableSpaceUsingBind = true)>]
            member x.PutToDifferentUri (monad, m) = 
                x.Set (monad, decisionPLens D.PutToDifferentUri, m)

            [<CustomOperation (D.RespondWithEntity, MaintainsVariableSpaceUsingBind = true)>]
            member x.RespondWithEntity (monad, m) = 
                x.Set (monad, decisionPLens D.RespondWithEntity, m)

            [<CustomOperation (D.ServiceAvailable, MaintainsVariableSpaceUsingBind = true)>]
            member x.ServiceAvailable (monad, m) = 
                x.Set (monad, decisionPLens D.ServiceAvailable, m)

            [<CustomOperation (D.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
            member x.UriTooLong (monad, m) = 
                x.Set (monad, decisionPLens D.UriTooLong, m)

    
    [<AutoOpen>]
    module Handlers =

        type MachineMonadBuilder with

            // 200

            [<CustomOperation (H.OK, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleOk (monad, m) = 
                x.Set (monad, handlerPLens H.OK, m)

            [<CustomOperation (H.Options, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleOptions (monad, m) = 
                x.Set (monad, handlerPLens H.Options, m)

            [<CustomOperation (H.Created, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleCreated (monad, m) = 
                x.Set (monad, handlerPLens H.Created, m)

            [<CustomOperation (H.Accepted, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleAccepted (monad, m) = 
                x.Set (monad, handlerPLens H.Accepted, m)

            [<CustomOperation (H.NoContent, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleNoContent (monad, m) = 
                x.Set (monad, handlerPLens H.NoContent, m)

            // 300

            [<CustomOperation (H.MovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleMovedPermanently (monad, m) = 
                x.Set (monad, handlerPLens H.MovedPermanently, m)

            [<CustomOperation (H.SeeOther, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleSeeOther (monad, m) = 
                x.Set (monad, handlerPLens H.SeeOther, m)

            [<CustomOperation (H.NotModified, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleNotModified (monad, m) = 
                x.Set (monad, handlerPLens H.NotModified, m)

            [<CustomOperation (H.MovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleMovedTemporarily (monad, m) = 
                x.Set (monad, handlerPLens H.MovedTemporarily, m)

            [<CustomOperation (H.MultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleMultipleRepresentations (monad, m) = 
                x.Set (monad, handlerPLens H.MultipleRepresentations, m)
        
            // 400

            [<CustomOperation (H.Malformed, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleMalformed (monad, m) = 
                x.Set (monad, handlerPLens H.Malformed, m)

            [<CustomOperation (H.Unauthorized, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleUnauthorized (monad, m) = 
                x.Set (monad, handlerPLens H.Unauthorized, m)

            [<CustomOperation (H.Forbidden, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleForbidden (monad, m) = 
                x.Set (monad, handlerPLens H.Forbidden, m)

            [<CustomOperation (H.NotFound, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleNotFound (monad, m) = 
                x.Set (monad, handlerPLens H.NotFound, m)

            [<CustomOperation (H.MethodNotAllowed, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleMethodNotAllowed (monad, m) = 
                x.Set (monad, handlerPLens H.MethodNotAllowed, m)

            [<CustomOperation (H.NotAcceptable, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleNotAcceptable (monad, m) = 
                x.Set (monad, handlerPLens H.NotAcceptable, m)

            [<CustomOperation (H.Conflict, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleConflict (monad, m) = 
                x.Set (monad, handlerPLens H.Conflict, m)

            [<CustomOperation (H.Gone, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleGone (monad, m) = 
                x.Set (monad, handlerPLens H.Gone, m)

            [<CustomOperation (H.PreconditionFailed, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandlePreconditionFailed (monad, m) = 
                x.Set (monad, handlerPLens H.PreconditionFailed, m)

            [<CustomOperation (H.RequestEntityTooLarge, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleRequestEntityTooLarge (monad, m) = 
                x.Set (monad, handlerPLens H.RequestEntityTooLarge, m)

            [<CustomOperation (H.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleUriTooLong (monad, m) = 
                x.Set (monad, handlerPLens H.UriTooLong, m)

            [<CustomOperation (H.UnsupportedMediaType, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleUnsupportedMediaType (monad, m) = 
                x.Set (monad, handlerPLens H.UnsupportedMediaType, m)

            [<CustomOperation (H.UnprocessableEntity, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleUnprocessableEntity (monad, m) = 
                x.Set (monad, handlerPLens H.UnprocessableEntity, m)

            // 500

            [<CustomOperation (H.Exception, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleException (monad, m) = 
                x.Set (monad, handlerPLens H.Exception, m)

            [<CustomOperation (H.NotImplemented, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleNotImplemented (monad, m) = 
                x.Set (monad, handlerPLens H.NotImplemented, m)

            [<CustomOperation (H.UnknownMethod, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleUnknownMethod (monad, m) = 
                x.Set (monad, handlerPLens H.UnknownMethod, m)
    
            [<CustomOperation (H.ServiceUnavailable, MaintainsVariableSpaceUsingBind = true)>]
            member x.HandleServiceUnavailable (monad, m) = 
                x.Set (monad, handlerPLens H.ServiceUnavailable, m)


    [<AutoOpen>]
    module Utility =

        type MachineMonadBuilder with
    
            [<CustomOperation ("including", MaintainsVariableSpaceUsingBind = true)>]
            member x.Including (monad, h) = 
                x.Combine (monad, h)
