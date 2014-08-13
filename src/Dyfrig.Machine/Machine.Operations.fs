namespace Dyfrig.Machine

open System
open Dyfrig
open Dyfrig.Machine.Keys


module A = Actions
module C = Config
module D = Decisions
module H = Handlers


[<AutoOpen>]
module MachineOperations =

    // Actions

    type MachineMonadBuilder with

        [<CustomOperation (A.Delete, MaintainsVariableSpaceUsingBind = true)>]
        member x.DoDelete (r, f) = x.Set (r, action A.Delete, Some f)

        [<CustomOperation (A.Patch, MaintainsVariableSpaceUsingBind = true)>]
        member x.DoPatch (r, f) = x.Set (r, action A.Patch, Some f)

        [<CustomOperation (A.Post, MaintainsVariableSpaceUsingBind = true)>]
        member x.DoPost (r, f) = x.Set (r, action A.Post, Some f)

        [<CustomOperation (A.Put, MaintainsVariableSpaceUsingBind = true)>]
        member x.DoPut (r, f) = x.Set (r, action A.Put, Some f)

    // Configuration

    type MachineMonadBuilder with

        [<CustomOperation (C.AllowedMethods, MaintainsVariableSpaceUsingBind = true)>]
        member x.AllowedMethods (r, methods: Method list) = x.Set (r, config C.AllowedMethods, Some (box  (Set methods)))

        [<CustomOperation (C.AvailableCharsets, MaintainsVariableSpaceUsingBind = true)>]
        member x.AvailableCharsets (r, charsets: string list) = x.Set (r, config C.AvailableCharsets, Some (box charsets))

        [<CustomOperation (C.AvailableEncodings, MaintainsVariableSpaceUsingBind = true)>]
        member x.AvailableEncodings (r, encodings: string list) = x.Set (r, config C.AvailableEncodings, Some (box encodings))

        [<CustomOperation (C.AvailableLanguages, MaintainsVariableSpaceUsingBind = true)>]
        member x.AvailableLanguages (r, languages: string list) = x.Set (r, config C.AvailableLanguages, Some (box languages))

        [<CustomOperation (C.AvailableMediaTypes, MaintainsVariableSpaceUsingBind = true)>]
        member x.AvailableMediaTypes (r, mediaTypes: string list) = x.Set (r, config C.AvailableMediaTypes, Some (box mediaTypes))

        [<CustomOperation (C.ETag, MaintainsVariableSpaceUsingBind = true)>]
        member x.ETag (r, etag: OwinMonad<string>) = x.Set (r, config C.ETag, Some (box etag))

        [<CustomOperation (C.KnownMethods, MaintainsVariableSpaceUsingBind = true)>]
        member x.KnownMethods (r, methods: Method list) = x.Set (r, config C.KnownMethods, Some (box (Set methods)))

        [<CustomOperation (C.Modified, MaintainsVariableSpaceUsingBind = true)>]
        member x.Modified (r, modified: OwinMonad<DateTime>) = x.Set (r, config C.Modified, Some (box modified))


    // Handlers

    type MachineMonadBuilder with

        // 200

        [<CustomOperation (H.OK, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleOk (r, f) = x.Set (r, handler H.OK, Some f)

        [<CustomOperation (H.Created, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleCreated (r, f) = x.Set (r, handler H.Created, Some f)

        [<CustomOperation (H.Options, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleOptions (r, f) = x.Set (r, handler H.Options, Some f)

        [<CustomOperation (H.Accepted, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleAccepted (r, f) = x.Set (r, handler H.Accepted, Some f)

        [<CustomOperation (H.NoContent, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNoContent (r, f) = x.Set (r, handler H.NoContent, Some f)

        // 300

        [<CustomOperation (H.MovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMovedPermanently (r, f) = x.Set (r, handler H.MovedPermanently, Some f)

        [<CustomOperation (H.SeeOther, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleSeeOther (r, f) = x.Set (r, handler H.SeeOther, Some f)

        [<CustomOperation (H.NotModified, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNotModified (r, f) = x.Set (r, handler H.NotModified, Some f)

        [<CustomOperation (H.MovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMovedTemporarily (r, f) = x.Set (r, handler H.MovedTemporarily, Some f)

        [<CustomOperation (H.MultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMultipleRepresentations (r, f) = x.Set (r, handler H.MultipleRepresentations, Some f)

        // 400

        [<CustomOperation (H.Malformed, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMalformed (r, f) = x.Set (r, handler H.Malformed, Some f)

        [<CustomOperation (H.Unauthorized, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUnauthorized (r, f) = x.Set (r, handler H.Unauthorized, Some f)

        [<CustomOperation (H.Forbidden, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleForbidden (r, f) = x.Set (r, handler H.Forbidden, Some f)

        [<CustomOperation (H.NotFound, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNotFound (r, f) = x.Set (r, handler H.NotFound, Some f)

        [<CustomOperation (H.MethodNotAllowed, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMethodNotAllowed (r, f) = x.Set (r, handler H.MethodNotAllowed, Some f)

        [<CustomOperation (H.NotAcceptable, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNotAcceptable (r, f) = x.Set (r, handler H.NotAcceptable, Some f)

        [<CustomOperation (H.Conflict, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleConflict (r, f) = x.Set (r, handler H.Conflict, Some f)

        [<CustomOperation (H.Gone, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleGone (r, f) = x.Set (r, handler H.Gone, Some f)

        [<CustomOperation (H.PreconditionFailed, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandlePreconditionFailed (r, f) = x.Set (r, handler H.PreconditionFailed, Some f)

        [<CustomOperation (H.RequestEntityTooLarge, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleRequestEntityTooLarge (r, f) = x.Set (r, handler H.RequestEntityTooLarge, Some f)

        [<CustomOperation (H.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUriTooLong (r, f) = x.Set (r, handler H.UriTooLong, Some f)

        [<CustomOperation (H.UnsupportedMediaType, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUnsupportedMediaType (r, f) = x.Set (r, handler H.UnsupportedMediaType, Some f)

        [<CustomOperation (H.UnprocessableEntity, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUnprocessableEntity (r, f) = x.Set (r, handler H.UnprocessableEntity, Some f)

        // 500

        [<CustomOperation (H.Exception, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleException (r, f) = x.Set (r, handler H.Exception, Some f)

        [<CustomOperation (H.NotImplemented, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNotImplemented (r, f) = x.Set (r, handler H.NotImplemented, Some f)

        [<CustomOperation (H.UnknownMethod, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUnknownMethod (r, f) = x.Set (r, handler H.UnknownMethod, Some f)
    
        [<CustomOperation (H.ServiceUnavailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleServiceUnavailable (r, f) = x.Set (r, handler H.ServiceUnavailable, Some f)

    // Decisions

    type MachineMonadBuilder with

        [<CustomOperation (D.Allowed, MaintainsVariableSpaceUsingBind = true)>]
        member x.Allowed (r, f) = x.Set (r, decision D.Allowed, Some f)

        [<CustomOperation (D.Authorized, MaintainsVariableSpaceUsingBind = true)>]
        member x.Authorized (r, f) = x.Set (r, decision D.Authorized, Some f)

        [<CustomOperation (D.CharsetAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.CharsetAvailable (r, f) = x.Set (r, decision D.CharsetAvailable, Some f)

        [<CustomOperation (D.CanPostToGone, MaintainsVariableSpaceUsingBind = true)>]
        member x.CanPostToGone (r, f) = x.Set (r, decision D.CanPostToGone, Some f)

        [<CustomOperation (D.CanPostToMissing, MaintainsVariableSpaceUsingBind = true)>]
        member x.CanPostToMissing (r, f) = x.Set (r, decision D.CanPostToMissing, Some f)

        [<CustomOperation (D.CanPutToMissing, MaintainsVariableSpaceUsingBind = true)>]
        member x.CanPutToMissing (r, f) = x.Set (r, decision D.CanPutToMissing, Some f)

        [<CustomOperation (D.ContentTypeKnown, MaintainsVariableSpaceUsingBind = true)>]
        member x.ContentTypeKnown (r, f) = x.Set (r, decision D.ContentTypeKnown, Some f)

        [<CustomOperation (D.ContentTypeValid, MaintainsVariableSpaceUsingBind = true)>]
        member x.ContentTypeValid (r, f) = x.Set (r, decision D.ContentTypeValid, Some f)

        [<CustomOperation (D.Conflict, MaintainsVariableSpaceUsingBind = true)>]
        member x.Conflict (r, f) = x.Set (r, decision D.Conflict, Some f)

        [<CustomOperation (D.Created, MaintainsVariableSpaceUsingBind = true)>]
        member x.Created (r, f) = x.Set (r, decision D.Created, Some f)

        [<CustomOperation (D.Deleted, MaintainsVariableSpaceUsingBind = true)>]
        member x.Deleted (r, f) = x.Set (r, decision D.Deleted, Some f)

        [<CustomOperation (D.EncodingAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.EncodingAvailable (r, f) = x.Set (r, decision D.EncodingAvailable, Some f)

        [<CustomOperation (D.ETagMatchesIf, MaintainsVariableSpaceUsingBind = true)>]
        member x.ETagMatchesIf (r, f) = x.Set (r, decision D.ETagMatchesIf, Some f)

        [<CustomOperation (D.ETagMatchesIfNone, MaintainsVariableSpaceUsingBind = true)>]
        member x.ETagMatchesIfNone (r, f) = x.Set (r, decision D.ETagMatchesIfNone, Some f)

        [<CustomOperation (D.Existed, MaintainsVariableSpaceUsingBind = true)>]
        member x.Existed (r, f) = x.Set (r, decision D.Existed, Some f)

        [<CustomOperation (D.Exists, MaintainsVariableSpaceUsingBind = true)>]
        member x.Exists (r, f) = x.Set (r, decision D.Exists, Some f)

        [<CustomOperation (D.LanguageAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.LanguageAvailable (r, f) = x.Set (r, decision D.LanguageAvailable, Some f)

        [<CustomOperation (D.MethodAllowed, MaintainsVariableSpaceUsingBind = true)>]
        member x.MethodAllowed (r, f) = x.Set (r, decision D.MethodAllowed, Some f)

        [<CustomOperation (D.MethodKnown, MaintainsVariableSpaceUsingBind = true)>]
        member x.MethodKnown (r, f) = x.Set (r, decision D.MethodKnown, Some f)

        [<CustomOperation (D.Malformed, MaintainsVariableSpaceUsingBind = true)>]
        member x.Malformed (r, f) = x.Set (r, decision D.Malformed, Some f)

        [<CustomOperation (D.MediaTypeAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.MediaTypeAvailable (r, f) = x.Set (r, decision D.MediaTypeAvailable, Some f)

        [<CustomOperation (D.MovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
        member x.MovedPermanently (r, f) = x.Set (r, decision D.MovedPermanently, Some f)

        [<CustomOperation (D.MovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
        member x.MovedTemporarily (r, f) = x.Set (r, decision D.MovedTemporarily, Some f)

        [<CustomOperation (D.MultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
        member x.MultipleRepresentations (r, f) = x.Set (r, decision D.MultipleRepresentations, Some f)

        [<CustomOperation (D.PostRedirect, MaintainsVariableSpaceUsingBind = true)>]
        member x.PostRedirect (r, f) = x.Set (r, decision D.PostRedirect, Some f)

        [<CustomOperation (D.Processable, MaintainsVariableSpaceUsingBind = true)>]
        member x.Processable (r, f) = x.Set (r, decision D.Processable, Some f)

        [<CustomOperation (D.PutToDifferentUri, MaintainsVariableSpaceUsingBind = true)>]
        member x.PutToDifferentUri (r, f) = x.Set (r, decision D.PutToDifferentUri, Some f)

        [<CustomOperation (D.RespondWithEntity, MaintainsVariableSpaceUsingBind = true)>]
        member x.RespondWithEntity (r, f) = x.Set (r, decision D.RespondWithEntity, Some f)

        [<CustomOperation (D.ServiceAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.ServiceAvailable (r, f) = x.Set (r, decision D.ServiceAvailable, Some f)

        [<CustomOperation (D.UnmodifiedSince, MaintainsVariableSpaceUsingBind = true)>]
        member x.UnmodifiedSince (r, f) = x.Set (r, decision D.UnmodifiedSince, Some f)

        [<CustomOperation (D.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
        member x.UriTooLong (r, f) = x.Set (r, decision D.UriTooLong, Some f)

        [<CustomOperation (D.ValidEntityLength, MaintainsVariableSpaceUsingBind = true)>]
        member x.ValidEntityLength (r, f) = x.Set (r, decision D.ValidEntityLength, Some f)

    // Utility

    type MachineMonadBuilder with
    
        [<CustomOperation ("including", MaintainsVariableSpaceUsingBind = true)>]
        member x.Including (r, h) = x.Combine (r, h)
        

//[<AutoOpen>]
//module RoutesOperations =
//
//    type FrostRoutesBuilder with
//    
//        [<CustomOperation ("resource", MaintainsVariableSpaceUsingBind = true)>]
//        member x.Resource (r, p, res) = x.Register (r, p, compileResource res)