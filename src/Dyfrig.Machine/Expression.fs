namespace Dyfrig.Machine

open System
open Aether
open Aether.Operators
open Dyfrig
open FSharpx

module A = Actions
module C = Config
module D = Decisions
module H = Handlers

type MachineDefinition =
    { Actions: Map<string, MachineAction>
      Configuration: Map<string, obj>
      Decisions: Map<string, MachineDecision> 
      Handlers: Map<string, MachineHandler> }

    static member empty =
        { Actions = Map.empty
          Configuration = Map.empty
          Decisions = Map.empty
          Handlers = Map.empty }

    static member actionsPLens k =
        ((fun x -> x.Actions), (fun a x -> { x with Actions = a }))
        >-?> mapPLens k
    
    static member configPLens<'T> k =
        ((fun x -> x.Configuration), (fun c x -> { x with Configuration = c }))
        >-?> mapPLens k
        >?-> isoBoxLens<'T>
        
    static member decisionsPLens k =
        ((fun x -> x.Decisions), (fun d x -> { x with Decisions = d }))
        >-?> mapPLens k

    static member handlersPLens k =
        ((fun x -> x.Handlers), (fun h x -> { x with Handlers = h }))
        >-?> mapPLens k

and MachineAction = 
    OwinMonad<unit>

and MachineDecision = 
    OwinMonad<bool>

and MachineHandler = 
    OwinMonad<byte []>

type MachineMonad = 
    MachineDefinition -> unit * MachineDefinition

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
            x.Set (r, MachineDefinition.actionsPLens A.Delete, f)

        [<CustomOperation (A.Patch, MaintainsVariableSpaceUsingBind = true)>]
        member x.DoPatch (r, f) = 
            x.Set (r, MachineDefinition.actionsPLens A.Patch, f)

        [<CustomOperation (A.Post, MaintainsVariableSpaceUsingBind = true)>]
        member x.DoPost (r, f) = 
            x.Set (r, MachineDefinition.actionsPLens A.Post, f)

        [<CustomOperation (A.Put, MaintainsVariableSpaceUsingBind = true)>]
        member x.DoPut (r, f) = 
            x.Set (r, MachineDefinition.actionsPLens A.Put, f)

[<AutoOpen>]
module ConfigurationOperations =

    type MachineMonadBuilder with

        [<CustomOperation (C.AllowedMethods, MaintainsVariableSpaceUsingBind = true)>]
        member x.AllowedMethods (r, methods: Method list) = 
            x.Set (r, MachineDefinition.configPLens C.AllowedMethods, Set methods)

        [<CustomOperation (C.AvailableCharsets, MaintainsVariableSpaceUsingBind = true)>]
        member x.AvailableCharsets (r, charsets: string list) = 
            x.Set (r, MachineDefinition.configPLens C.AvailableCharsets, charsets)

        [<CustomOperation (C.AvailableEncodings, MaintainsVariableSpaceUsingBind = true)>]
        member x.AvailableEncodings (r, encodings: string list) = 
            x.Set (r, MachineDefinition.configPLens C.AvailableEncodings, encodings)

        [<CustomOperation (C.AvailableLanguages, MaintainsVariableSpaceUsingBind = true)>]
        member x.AvailableLanguages (r, languages: string list) = 
            x.Set (r, MachineDefinition.configPLens C.AvailableLanguages, languages)

        [<CustomOperation (C.AvailableMediaTypes, MaintainsVariableSpaceUsingBind = true)>]
        member x.AvailableMediaTypes (r, mediaTypes: string list) =
            x.Set (r, MachineDefinition.configPLens C.AvailableMediaTypes, mediaTypes)

        [<CustomOperation (C.ETag, MaintainsVariableSpaceUsingBind = true)>]
        member x.ETag (r, etag: OwinMonad<string>) = 
            x.Set (r, MachineDefinition.configPLens C.ETag, etag)

        [<CustomOperation (C.KnownMethods, MaintainsVariableSpaceUsingBind = true)>]
        member x.KnownMethods (r, methods: Method list) = 
            x.Set (r, MachineDefinition.configPLens C.KnownMethods, Set methods)

        [<CustomOperation (C.Modified, MaintainsVariableSpaceUsingBind = true)>]
        member x.Modified (r, modified: OwinMonad<DateTime>) = 
            x.Set (r, MachineDefinition.configPLens C.Modified, modified)

[<AutoOpen>]
module DecisionOperations =

    type MachineMonadBuilder with

        [<CustomOperation (D.Allowed, MaintainsVariableSpaceUsingBind = true)>]
        member x.Allowed (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.Allowed, f)

        [<CustomOperation (D.Authorized, MaintainsVariableSpaceUsingBind = true)>]
        member x.Authorized (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.Authorized, f)

        [<CustomOperation (D.CharsetAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.CharsetAvailable (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.CharsetAvailable, f)

        [<CustomOperation (D.CanPostToGone, MaintainsVariableSpaceUsingBind = true)>]
        member x.CanPostToGone (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.CanPostToGone, f)

        [<CustomOperation (D.CanPostToMissing, MaintainsVariableSpaceUsingBind = true)>]
        member x.CanPostToMissing (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.CanPostToMissing, f)

        [<CustomOperation (D.CanPutToMissing, MaintainsVariableSpaceUsingBind = true)>]
        member x.CanPutToMissing (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.CanPutToMissing, f)

        [<CustomOperation (D.ContentTypeKnown, MaintainsVariableSpaceUsingBind = true)>]
        member x.ContentTypeKnown (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.ContentTypeKnown, f)

        [<CustomOperation (D.ContentTypeValid, MaintainsVariableSpaceUsingBind = true)>]
        member x.ContentTypeValid (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.ContentTypeValid, f)

        [<CustomOperation (D.Conflict, MaintainsVariableSpaceUsingBind = true)>]
        member x.Conflict (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.Conflict, f)

        [<CustomOperation (D.Created, MaintainsVariableSpaceUsingBind = true)>]
        member x.Created (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.Created, f)

        [<CustomOperation (D.Deleted, MaintainsVariableSpaceUsingBind = true)>]
        member x.Deleted (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.Deleted, f)

        [<CustomOperation (D.EncodingAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.EncodingAvailable (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.EncodingAvailable, f)

        [<CustomOperation (D.ETagMatchesIf, MaintainsVariableSpaceUsingBind = true)>]
        member x.ETagMatchesIf (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.ETagMatchesIf, f)

        [<CustomOperation (D.ETagMatchesIfNone, MaintainsVariableSpaceUsingBind = true)>]
        member x.ETagMatchesIfNone (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.ETagMatchesIfNone, f)

        [<CustomOperation (D.Existed, MaintainsVariableSpaceUsingBind = true)>]
        member x.Existed (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.Existed, f)

        [<CustomOperation (D.Exists, MaintainsVariableSpaceUsingBind = true)>]
        member x.Exists (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.Exists, f)

        [<CustomOperation (D.LanguageAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.LanguageAvailable (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.LanguageAvailable, f)

        [<CustomOperation (D.MethodAllowed, MaintainsVariableSpaceUsingBind = true)>]
        member x.MethodAllowed (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.MethodAllowed, f)

        [<CustomOperation (D.MethodKnown, MaintainsVariableSpaceUsingBind = true)>]
        member x.MethodKnown (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.MethodKnown, f)

        [<CustomOperation (D.Malformed, MaintainsVariableSpaceUsingBind = true)>]
        member x.Malformed (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.Malformed, f)

        [<CustomOperation (D.MediaTypeAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.MediaTypeAvailable (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.MediaTypeAvailable, f)

        [<CustomOperation (D.MovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
        member x.MovedPermanently (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.MovedPermanently, f)

        [<CustomOperation (D.MovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
        member x.MovedTemporarily (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.MovedTemporarily, f)

        [<CustomOperation (D.MultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
        member x.MultipleRepresentations (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.MultipleRepresentations, f)

        [<CustomOperation (D.PostRedirect, MaintainsVariableSpaceUsingBind = true)>]
        member x.PostRedirect (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.PostRedirect, f)

        [<CustomOperation (D.Processable, MaintainsVariableSpaceUsingBind = true)>]
        member x.Processable (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.Processable, f)

        [<CustomOperation (D.PutToDifferentUri, MaintainsVariableSpaceUsingBind = true)>]
        member x.PutToDifferentUri (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.PutToDifferentUri, f)

        [<CustomOperation (D.RespondWithEntity, MaintainsVariableSpaceUsingBind = true)>]
        member x.RespondWithEntity (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.RespondWithEntity, f)

        [<CustomOperation (D.ServiceAvailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.ServiceAvailable (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.ServiceAvailable, f)

        [<CustomOperation (D.UnmodifiedSince, MaintainsVariableSpaceUsingBind = true)>]
        member x.UnmodifiedSince (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.UnmodifiedSince, f)

        [<CustomOperation (D.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
        member x.UriTooLong (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.UriTooLong, f)

        [<CustomOperation (D.ValidEntityLength, MaintainsVariableSpaceUsingBind = true)>]
        member x.ValidEntityLength (r, f) = 
            x.Set (r, MachineDefinition.decisionsPLens D.ValidEntityLength, f)

[<AutoOpen>]
module HandlerOperations =

    type MachineMonadBuilder with

        // 200

        [<CustomOperation (H.OK, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleOk (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.OK, f)

        [<CustomOperation (H.Created, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleCreated (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.Created, f)

        [<CustomOperation (H.Options, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleOptions (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.Options, f)

        [<CustomOperation (H.Accepted, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleAccepted (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.Accepted, f)

        [<CustomOperation (H.NoContent, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNoContent (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.NoContent, f)

        // 300

        [<CustomOperation (H.MovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMovedPermanently (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.MovedPermanently, f)

        [<CustomOperation (H.SeeOther, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleSeeOther (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.SeeOther, f)

        [<CustomOperation (H.NotModified, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNotModified (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.NotModified, f)

        [<CustomOperation (H.MovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMovedTemporarily (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.MovedTemporarily, f)

        [<CustomOperation (H.MultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMultipleRepresentations (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.MultipleRepresentations, f)
        
        // 400

        [<CustomOperation (H.Malformed, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMalformed (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.Malformed, f)

        [<CustomOperation (H.Unauthorized, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUnauthorized (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.Unauthorized, f)

        [<CustomOperation (H.Forbidden, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleForbidden (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.Forbidden, f)

        [<CustomOperation (H.NotFound, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNotFound (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.NotFound, f)

        [<CustomOperation (H.MethodNotAllowed, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleMethodNotAllowed (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.MethodNotAllowed, f)

        [<CustomOperation (H.NotAcceptable, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNotAcceptable (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.NotAcceptable, f)

        [<CustomOperation (H.Conflict, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleConflict (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.Conflict, f)

        [<CustomOperation (H.Gone, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleGone (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.Gone, f)

        [<CustomOperation (H.PreconditionFailed, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandlePreconditionFailed (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.PreconditionFailed, f)

        [<CustomOperation (H.RequestEntityTooLarge, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleRequestEntityTooLarge (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.RequestEntityTooLarge, f)

        [<CustomOperation (H.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUriTooLong (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.UriTooLong, f)

        [<CustomOperation (H.UnsupportedMediaType, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUnsupportedMediaType (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.UnsupportedMediaType, f)

        [<CustomOperation (H.UnprocessableEntity, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUnprocessableEntity (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.UnprocessableEntity, f)

        // 500

        [<CustomOperation (H.Exception, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleException (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.Exception, f)

        [<CustomOperation (H.NotImplemented, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleNotImplemented (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.NotImplemented, f)

        [<CustomOperation (H.UnknownMethod, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleUnknownMethod (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.UnknownMethod, f)
    
        [<CustomOperation (H.ServiceUnavailable, MaintainsVariableSpaceUsingBind = true)>]
        member x.HandleServiceUnavailable (r, f) = 
            x.Set (r, MachineDefinition.handlersPLens H.ServiceUnavailable, f)

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
