namespace Dyfrig.Machine

open System
open Aether
open Dyfrig
open FSharpx

module A = Actions
module C = Config
module D = Decisions
module H = Handlers

type MachineMonad = 
    Definition -> unit * Definition

type MachineMonadBuilder () =

    member x.Return (t) : MachineMonad = 
        tuple2 t

    member x.ReturnFrom f : MachineMonad = 
        f

    member x.Bind (m, k) : MachineMonad = 
        m >> fun (result, resource) -> (k result) resource

    member x.Combine (r1, r2) : MachineMonad = 
        x.Bind (r1, fun () -> r2)

    member internal x.Set (r, lens, value) = 
        x.Bind ((fun res -> (), setPL lens value res), fun _ -> x.ReturnFrom r)

[<AutoOpen>]
module Expression =

    let machine = MachineMonadBuilder ()

[<AutoOpen>]
module ActionOperations =

    type MachineMonadBuilder with

        [<CustomOperation (A.Delete, MaintainsVariableSpaceUsingBind = true)>]
        member x.DoDelete (r, f) = 
            x.Set (r, Definition.actionsPLens A.Delete, f)

        [<CustomOperation (A.Patch, MaintainsVariableSpaceUsingBind = true)>]
        member x.DoPatch (r, f) = 
            x.Set (r, Definition.actionsPLens A.Patch, f)

        [<CustomOperation (A.Post, MaintainsVariableSpaceUsingBind = true)>]
        member x.DoPost (r, f) = 
            x.Set (r, Definition.actionsPLens A.Post, f)

        [<CustomOperation (A.Put, MaintainsVariableSpaceUsingBind = true)>]
        member x.DoPut (r, f) = 
            x.Set (r, Definition.actionsPLens A.Put, f)

[<AutoOpen>]
module ConfigurationOperations =

    type MachineMonadBuilder with

        [<CustomOperation (C.AllowedMethods, MaintainsVariableSpaceUsingBind = true)>]
        member x.AllowedMethods (r, methods: Method list) = 
            x.Set (r, Definition.configPLens C.AllowedMethods, Set methods)

        [<CustomOperation (C.AvailableCharsets, MaintainsVariableSpaceUsingBind = true)>]
        member x.AvailableCharsets (r, charsets: string list) = 
            x.Set (r, Definition.configPLens C.AvailableCharsets, charsets)

        [<CustomOperation (C.AvailableEncodings, MaintainsVariableSpaceUsingBind = true)>]
        member x.AvailableEncodings (r, encodings: string list) = 
            x.Set (r, Definition.configPLens C.AvailableEncodings, encodings)

        [<CustomOperation (C.AvailableLanguages, MaintainsVariableSpaceUsingBind = true)>]
        member x.AvailableLanguages (r, languages: string list) = 
            x.Set (r, Definition.configPLens C.AvailableLanguages, languages)

        [<CustomOperation (C.AvailableMediaTypes, MaintainsVariableSpaceUsingBind = true)>]
        member x.AvailableMediaTypes (r, mediaTypes: string list) =
            x.Set (r, Definition.configPLens C.AvailableMediaTypes, mediaTypes)

        [<CustomOperation (C.ETag, MaintainsVariableSpaceUsingBind = true)>]
        member x.ETag (r, etag: OwinMonad<string>) = 
            x.Set (r, Definition.configPLens C.ETag, etag)

        [<CustomOperation (C.KnownMethods, MaintainsVariableSpaceUsingBind = true)>]
        member x.KnownMethods (r, methods: Method list) = 
            x.Set (r, Definition.configPLens C.KnownMethods, Set methods)

        [<CustomOperation (C.Modified, MaintainsVariableSpaceUsingBind = true)>]
        member x.Modified (r, modified: OwinMonad<DateTime>) = 
            x.Set (r, Definition.configPLens C.Modified, modified)

[<AutoOpen>]
module HandlerOperations =

    type MachineMonadBuilder with

        // 200

        [<CustomOperation (H.OK, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleOk (r, f) = 
            x.Set (r, Definition.handlersPLens H.OK, f)

        [<CustomOperation (H.Created, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleCreated (r, f) = 
            x.Set (r, Definition.handlersPLens H.Created, f)

        [<CustomOperation (H.Options, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleOptions (r, f) = 
            x.Set (r, Definition.handlersPLens H.Options, f)

        [<CustomOperation (H.Accepted, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleAccepted (r, f) = 
            x.Set (r, Definition.handlersPLens H.Accepted, f)

        [<CustomOperation (H.NoContent, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNoContent (r, f) = 
            x.Set (r, Definition.handlersPLens H.NoContent, f)

        // 300

        [<CustomOperation (H.MovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMovedPermanently (r, f) = 
            x.Set (r, Definition.handlersPLens H.MovedPermanently, f)

        [<CustomOperation (H.SeeOther, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleSeeOther (r, f) = 
            x.Set (r, Definition.handlersPLens H.SeeOther, f)

        [<CustomOperation (H.NotModified, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNotModified (r, f) = 
            x.Set (r, Definition.handlersPLens H.NotModified, f)

        [<CustomOperation (H.MovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMovedTemporarily (r, f) = 
            x.Set (r, Definition.handlersPLens H.MovedTemporarily, f)

        [<CustomOperation (H.MultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMultipleRepresentations (r, f) = 
            x.Set (r, Definition.handlersPLens H.MultipleRepresentations, f)
        
        // 400

        [<CustomOperation (H.Malformed, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMalformed (r, f) = 
            x.Set (r, Definition.handlersPLens H.Malformed, f)

        [<CustomOperation (H.Unauthorized, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUnauthorized (r, f) = 
            x.Set (r, Definition.handlersPLens H.Unauthorized, f)

        [<CustomOperation (H.Forbidden, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleForbidden (r, f) = 
            x.Set (r, Definition.handlersPLens H.Forbidden, f)

        [<CustomOperation (H.NotFound, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNotFound (r, f) = 
            x.Set (r, Definition.handlersPLens H.NotFound, f)

        [<CustomOperation (H.MethodNotAllowed, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMethodNotAllowed (r, f) = 
            x.Set (r, Definition.handlersPLens H.MethodNotAllowed, f)

        [<CustomOperation (H.NotAcceptable, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNotAcceptable (r, f) = 
            x.Set (r, Definition.handlersPLens H.NotAcceptable, f)

        [<CustomOperation (H.Conflict, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleConflict (r, f) = 
            x.Set (r, Definition.handlersPLens H.Conflict, f)

        [<CustomOperation (H.Gone, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleGone (r, f) = 
            x.Set (r, Definition.handlersPLens H.Gone, f)

        [<CustomOperation (H.PreconditionFailed, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandlePreconditionFailed (r, f) = 
            x.Set (r, Definition.handlersPLens H.PreconditionFailed, f)

        [<CustomOperation (H.RequestEntityTooLarge, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleRequestEntityTooLarge (r, f) = 
            x.Set (r, Definition.handlersPLens H.RequestEntityTooLarge, f)

        [<CustomOperation (H.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUriTooLong (r, f) = 
            x.Set (r, Definition.handlersPLens H.UriTooLong, f)

        [<CustomOperation (H.UnsupportedMediaType, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUnsupportedMediaType (r, f) = 
            x.Set (r, Definition.handlersPLens H.UnsupportedMediaType, f)

        [<CustomOperation (H.UnprocessableEntity, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUnprocessableEntity (r, f) = 
            x.Set (r, Definition.handlersPLens H.UnprocessableEntity, f)

        // 500

        [<CustomOperation (H.Exception, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleException (r, f) = 
            x.Set (r, Definition.handlersPLens H.Exception, f)

        [<CustomOperation (H.NotImplemented, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNotImplemented (r, f) = 
            x.Set (r, Definition.handlersPLens H.NotImplemented, f)

        [<CustomOperation (H.UnknownMethod, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUnknownMethod (r, f) = 
            x.Set (r, Definition.handlersPLens H.UnknownMethod, f)
    
        [<CustomOperation (H.ServiceUnavailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleServiceUnavailable (r, f) = 
            x.Set (r, Definition.handlersPLens H.ServiceUnavailable, f)

[<AutoOpen>]
module DecisionOperations =

    type MachineMonadBuilder with

        [<CustomOperation (D.Allowed, MaintainsVariableSpaceUsingBind = true)>]
        member x.Allowed (r, f) = 
            x.Set (r, Definition.decisionsPLens D.Allowed, f)

        [<CustomOperation (D.Authorized, MaintainsVariableSpaceUsingBind = true)>]
        member x.Authorized (r, f) = 
            x.Set (r, Definition.decisionsPLens D.Authorized, f)

        [<CustomOperation (D.CharsetAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.CharsetAvailable (r, f) = 
            x.Set (r, Definition.decisionsPLens D.CharsetAvailable, f)

        [<CustomOperation (D.CanPostToGone, MaintainsVariableSpaceUsingBind = true)>]
        member x.CanPostToGone (r, f) = 
            x.Set (r, Definition.decisionsPLens D.CanPostToGone, f)

        [<CustomOperation (D.CanPostToMissing, MaintainsVariableSpaceUsingBind = true)>]
        member x.CanPostToMissing (r, f) = 
            x.Set (r, Definition.decisionsPLens D.CanPostToMissing, f)

        [<CustomOperation (D.CanPutToMissing, MaintainsVariableSpaceUsingBind = true)>]
        member x.CanPutToMissing (r, f) = 
            x.Set (r, Definition.decisionsPLens D.CanPutToMissing, f)

        [<CustomOperation (D.ContentTypeKnown, MaintainsVariableSpaceUsingBind = true)>]
        member x.ContentTypeKnown (r, f) = 
            x.Set (r, Definition.decisionsPLens D.ContentTypeKnown, f)

        [<CustomOperation (D.ContentTypeValid, MaintainsVariableSpaceUsingBind = true)>]
        member x.ContentTypeValid (r, f) = 
            x.Set (r, Definition.decisionsPLens D.ContentTypeValid, f)

        [<CustomOperation (D.Conflict, MaintainsVariableSpaceUsingBind = true)>]
        member x.Conflict (r, f) = 
            x.Set (r, Definition.decisionsPLens D.Conflict, f)

        [<CustomOperation (D.Created, MaintainsVariableSpaceUsingBind = true)>]
        member x.Created (r, f) = 
            x.Set (r, Definition.decisionsPLens D.Created, f)

        [<CustomOperation (D.Deleted, MaintainsVariableSpaceUsingBind = true)>]
        member x.Deleted (r, f) = 
            x.Set (r, Definition.decisionsPLens D.Deleted, f)

        [<CustomOperation (D.EncodingAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.EncodingAvailable (r, f) = 
            x.Set (r, Definition.decisionsPLens D.EncodingAvailable, f)

        [<CustomOperation (D.ETagMatchesIf, MaintainsVariableSpaceUsingBind = true)>]
        member x.ETagMatchesIf (r, f) = 
            x.Set (r, Definition.decisionsPLens D.ETagMatchesIf, f)

        [<CustomOperation (D.ETagMatchesIfNone, MaintainsVariableSpaceUsingBind = true)>]
        member x.ETagMatchesIfNone (r, f) = 
            x.Set (r, Definition.decisionsPLens D.ETagMatchesIfNone, f)

        [<CustomOperation (D.Existed, MaintainsVariableSpaceUsingBind = true)>]
        member x.Existed (r, f) = 
            x.Set (r, Definition.decisionsPLens D.Existed, f)

        [<CustomOperation (D.Exists, MaintainsVariableSpaceUsingBind = true)>]
        member x.Exists (r, f) = 
            x.Set (r, Definition.decisionsPLens D.Exists, f)

        [<CustomOperation (D.LanguageAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.LanguageAvailable (r, f) = 
            x.Set (r, Definition.decisionsPLens D.LanguageAvailable, f)

        [<CustomOperation (D.MethodAllowed, MaintainsVariableSpaceUsingBind = true)>]
        member x.MethodAllowed (r, f) = 
            x.Set (r, Definition.decisionsPLens D.MethodAllowed, f)

        [<CustomOperation (D.MethodKnown, MaintainsVariableSpaceUsingBind = true)>]
        member x.MethodKnown (r, f) = 
            x.Set (r, Definition.decisionsPLens D.MethodKnown, f)

        [<CustomOperation (D.Malformed, MaintainsVariableSpaceUsingBind = true)>]
        member x.Malformed (r, f) = 
            x.Set (r, Definition.decisionsPLens D.Malformed, f)

        [<CustomOperation (D.MediaTypeAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.MediaTypeAvailable (r, f) = 
            x.Set (r, Definition.decisionsPLens D.MediaTypeAvailable, f)

        [<CustomOperation (D.MovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
        member x.MovedPermanently (r, f) = 
            x.Set (r, Definition.decisionsPLens D.MovedPermanently, f)

        [<CustomOperation (D.MovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
        member x.MovedTemporarily (r, f) = 
            x.Set (r, Definition.decisionsPLens D.MovedTemporarily, f)

        [<CustomOperation (D.MultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
        member x.MultipleRepresentations (r, f) = 
            x.Set (r, Definition.decisionsPLens D.MultipleRepresentations, f)

        [<CustomOperation (D.PostRedirect, MaintainsVariableSpaceUsingBind = true)>]
        member x.PostRedirect (r, f) = 
            x.Set (r, Definition.decisionsPLens D.PostRedirect, f)

        [<CustomOperation (D.Processable, MaintainsVariableSpaceUsingBind = true)>]
        member x.Processable (r, f) = 
            x.Set (r, Definition.decisionsPLens D.Processable, f)

        [<CustomOperation (D.PutToDifferentUri, MaintainsVariableSpaceUsingBind = true)>]
        member x.PutToDifferentUri (r, f) = 
            x.Set (r, Definition.decisionsPLens D.PutToDifferentUri, f)

        [<CustomOperation (D.RespondWithEntity, MaintainsVariableSpaceUsingBind = true)>]
        member x.RespondWithEntity (r, f) = 
            x.Set (r, Definition.decisionsPLens D.RespondWithEntity, f)

        [<CustomOperation (D.ServiceAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.ServiceAvailable (r, f) = 
            x.Set (r, Definition.decisionsPLens D.ServiceAvailable, f)

        [<CustomOperation (D.UnmodifiedSince, MaintainsVariableSpaceUsingBind = true)>]
        member x.UnmodifiedSince (r, f) = 
            x.Set (r, Definition.decisionsPLens D.UnmodifiedSince, f)

        [<CustomOperation (D.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
        member x.UriTooLong (r, f) = 
            x.Set (r, Definition.decisionsPLens D.UriTooLong, f)

        [<CustomOperation (D.ValidEntityLength, MaintainsVariableSpaceUsingBind = true)>]
        member x.ValidEntityLength (r, f) = 
            x.Set (r, Definition.decisionsPLens D.ValidEntityLength, f)

[<AutoOpen>]
module UtilityOperations =

    type MachineMonadBuilder with
    
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
