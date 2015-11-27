//----------------------------------------------------------------------------
//
// Copyright (c) 2014
//
//    Ryan Riley (@panesofglass) and Andrew Cherry (@kolektiv)
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
//----------------------------------------------------------------------------

[<AutoOpen>]
module Freya.Machine.Extensions.Http.Syntax

open System
open Aether
open Arachne.Http
open Arachne.Language
open Arachne.Uri
open Freya.Core
open Freya.Machine

(* Helpers *)

[<RequireQualifiedAccess>]
module Configuration =

    let add<'a> key a =
        Lens.map FreyaMachineSpecification.Configuration_ (Configuration.set<'a> key (Some a))

(* Type Classes

   Static inference functions to allow for type-safe overloading of arguments
   to custom syntax operations. *)

(* Decisions *)

[<RequireQualifiedAccess>]
module Decision =

    type Defaults =
        | Defaults

        static member inline Decision (x: Freya<bool>) =
            x

        static member inline Decision (x: bool) =
            Freya.init x

    let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member Decision: ^a -> Freya<bool>) a)

    let inline infer (x: 'a) =
        defaults (x, Defaults)

(* Handlers *)

[<RequireQualifiedAccess>]
module Handler =

    type Defaults =
        | Defaults

        static member inline Handler (x: Specification -> Freya<Representation>) =
            x

        static member inline Handler (x: Freya<Representation>) =
            fun (_: Specification) -> x

        static member inline Handler (x: Representation) =
            fun (_: Specification) -> Freya.init x

    let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member Handler: ^a -> (Specification -> Freya<Representation>)) a)

    let inline infer (x: 'a) =
        defaults (x, Defaults)

(* Properties *)

[<RequireQualifiedAccess>]
module Charsets =

    type Defaults =
        | Defaults

        static member inline Charsets (x: Freya<Charset list>) =
            x

        static member inline Charsets (x: Charset list) =
            Freya.init x

        static member inline Charsets (x: Charset) =
            Freya.init [ x ]

    let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member Charsets: ^a -> Freya<Charset list>) a)

    let inline infer (x: 'a) =
        defaults (x, Defaults)

[<RequireQualifiedAccess>]
module ContentCodings =

    type Defaults =
        | Defaults

        static member inline ContentCodings (x: Freya<ContentCoding list>) =
            x

        static member inline ContentCodings (x: ContentCoding list) =
            Freya.init x

        static member inline ContentCodings (x: ContentCoding) =
            Freya.init [ x ]

    let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member ContentCodings: ^a -> Freya<ContentCoding list>) a)

    let inline infer (x: 'a) =
        defaults (x, Defaults)

[<RequireQualifiedAccess>]
module DateTime =

    type Defaults =
        | Defaults

        static member inline DateTime (x: Freya<DateTime>) =
            x

        static member inline DateTime (x: DateTime) =
            Freya.init x

    let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member DateTime: ^a -> Freya<DateTime>) a)

    let inline infer (x: 'a) =
        defaults (x, Defaults)


[<RequireQualifiedAccess>]
module ETag =

    type Defaults =
        | Defaults

        static member inline ETag (x: Freya<ETag>) =
            x

        static member inline ETag (x: ETag) =
            Freya.init x

    let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member ETag: ^a -> Freya<ETag>) a)

    let inline infer (x: 'a) =
        defaults (x, Defaults)

[<RequireQualifiedAccess>]
module LanguageTags =

    type Defaults =
        | Defaults

        static member inline LanguageTags (x: Freya<LanguageTag list>) =
            x

        static member inline LanguageTags (x: LanguageTag list) =
            Freya.init x

        static member inline LanguageTags (x: LanguageTag) =
            Freya.init [ x ]

    let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member LanguageTags: ^a -> Freya<LanguageTag list>) a)

    let inline infer (x: 'a) =
        defaults (x, Defaults)

[<RequireQualifiedAccess>]
module MediaTypes =

    type Defaults =
        | Defaults

        static member inline MediaTypes (x: Freya<MediaType list>) =
            x

        static member inline MediaTypes (x: MediaType list) =
            Freya.init x

        static member inline MediaTypes (x: MediaType) =
            Freya.init [ x ]

    let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member MediaTypes: ^a -> Freya<MediaType list>) a)

    let inline infer (x: 'a) =
        defaults (x, Defaults)


[<RequireQualifiedAccess>]
module Methods =

    type Defaults =
        | Defaults

        static member inline Methods (x: Freya<Method list>) =
            x

        static member inline Methods (x: Method list) =
            Freya.init x

        static member inline Methods (x: Method) =
            Freya.init [ x ]

    let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member Methods: ^a -> Freya<Method list>) a)

    let inline infer (x: 'a) =
        defaults (x, Defaults)

[<RequireQualifiedAccess>]
module UriReference =

    type Defaults =
        | Defaults

        static member inline UriReference (x: Freya<UriReference>) =
            x

        static member inline UriReference (x: UriReference) =
            Freya.init x

    let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member UriReference: ^a -> Freya<UriReference>) a)

    let inline infer (x: 'a) =
        defaults (x, Defaults)

(* Custom Operations

   Custom syntax operators used in the FreyaMachine computation
   expression. Custom syntax operators are used heavily and are the
   configuration mechanism for configuring a machine resource. *)

type FreyaMachineBuilder with

    (* Actions *)

    [<CustomOperation (Actions.Delete, MaintainsVariableSpaceUsingBind = true)>]
    member x.DoDelete (m, delete: Freya<unit>) =
        x.Map (m, Configuration.add Actions.Delete delete)

    [<CustomOperation (Actions.Patch, MaintainsVariableSpaceUsingBind = true)>]
    member x.DoPatch (m, patch: Freya<unit>) = 
        x.Map (m, Configuration.add Actions.Patch patch)

    [<CustomOperation (Actions.Post, MaintainsVariableSpaceUsingBind = true)>]
    member x.DoPost (m, post: Freya<unit>) = 
        x.Map (m, Configuration.add Actions.Post post)

    [<CustomOperation (Actions.Put, MaintainsVariableSpaceUsingBind = true)>]
    member x.DoPut (m, put: Freya<unit>) =
        x.Map (m, Configuration.add Actions.Put put)

    (* Decisions *)

    [<CustomOperation (Decisions.Allowed, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.Allowed (m, decision) = 
        x.Map (m, Configuration.add Decisions.Allowed (Decision.infer decision))

    [<CustomOperation (Decisions.AllowPostToGone, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.AllowPostToGone (m, decision) = 
        x.Map (m, Configuration.add Decisions.AllowPostToGone (Decision.infer decision))

    [<CustomOperation (Decisions.AllowPostToMissing, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.AllowPostToMissing (m, decision) = 
        x.Map (m, Configuration.add Decisions.AllowPostToMissing (Decision.infer decision))

    [<CustomOperation (Decisions.AllowPutToMissing, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.AllowPutToMissing (m, decision) = 
        x.Map (m, Configuration.add Decisions.AllowPutToMissing (Decision.infer decision))

    [<CustomOperation (Decisions.Authorized, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.Authorized (m, decision) = 
        x.Map (m, Configuration.add Decisions.Authorized (Decision.infer decision))

    [<CustomOperation (Decisions.CharsetsStrict, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.CharsetsStrict (m, decision) = 
        x.Map (m, Configuration.add Decisions.CharsetsStrict (Decision.infer decision))

    [<CustomOperation (Decisions.Conflicts, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.Conflicts (m, decision) = 
        x.Map (m, Configuration.add Decisions.Conflicts (Decision.infer decision))

    [<CustomOperation (Decisions.ContentTypeKnown, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.ContentTypeKnown (m, decision) = 
        x.Map (m, Configuration.add Decisions.ContentTypeKnown (Decision.infer decision))

    [<CustomOperation (Decisions.ContentTypeValid, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.ContentTypeValid (m, decision) = 
        x.Map (m, Configuration.add Decisions.ContentTypeValid (Decision.infer decision))

    [<CustomOperation (Decisions.Created, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.Created (m, decision) = 
        x.Map (m, Configuration.add Decisions.Created (Decision.infer decision))

    [<CustomOperation (Decisions.Deleted, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.Deleted (m, decision) = 
        x.Map (m, Configuration.add Decisions.Deleted (Decision.infer decision))

    [<CustomOperation (Decisions.EncodingsStrict, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.EncodingsStrict (m, decision) = 
        x.Map (m, Configuration.add Decisions.EncodingsStrict (Decision.infer decision))

    [<CustomOperation (Decisions.EntityLengthValid, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.EntityLengthValid (m, decision) = 
        x.Map (m, Configuration.add Decisions.EntityLengthValid (Decision.infer decision))

    [<CustomOperation (Decisions.Existed, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.Existed (m, decision) = 
        x.Map (m, Configuration.add Decisions.Existed (Decision.infer decision))

    [<CustomOperation (Decisions.Exists, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.Exists (m, decision) = 
        x.Map (m, Configuration.add Decisions.Exists (Decision.infer decision))

    [<CustomOperation (Decisions.LanguagesStrict, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.LanguagesStrict (m, decision) = 
        x.Map (m, Configuration.add Decisions.LanguagesStrict (Decision.infer decision))

    [<CustomOperation (Decisions.Malformed, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.Malformed (m, decision) = 
        x.Map (m, Configuration.add Decisions.Malformed (Decision.infer decision))

    [<CustomOperation (Decisions.MediaTypesStrict, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.MediaTypesStrict (m, decision) = 
        x.Map (m, Configuration.add Decisions.MediaTypesStrict (Decision.infer decision))

    [<CustomOperation (Decisions.MovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.MovedPermanently (m, decision) = 
        x.Map (m, Configuration.add Decisions.MovedPermanently (Decision.infer decision))

    [<CustomOperation (Decisions.MovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.MovedTemporarily (m, decision) = 
        x.Map (m, Configuration.add Decisions.MovedTemporarily (Decision.infer decision))

    [<CustomOperation (Decisions.MultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.MultipleRepresentations (m, decision) = 
        x.Map (m, Configuration.add Decisions.MultipleRepresentations (Decision.infer decision))

    [<CustomOperation (Decisions.PostRedirect, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.PostRedirect (m, decision) =
        x.Map (m, Configuration.add Decisions.PostRedirect (Decision.infer decision))

    [<CustomOperation (Decisions.Processable, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.Processable (m, decision) = 
        x.Map (m, Configuration.add Decisions.Processable (Decision.infer decision))

    [<CustomOperation (Decisions.PutToDifferentUri, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.PutToDifferentUri (m, decision) = 
        x.Map (m, Configuration.add Decisions.PutToDifferentUri (Decision.infer decision))

    [<CustomOperation (Decisions.RespondWithEntity, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.RespondWithEntity (m, decision) = 
        x.Map (m, Configuration.add Decisions.RespondWithEntity (Decision.infer decision))

    [<CustomOperation (Decisions.ServiceAvailable, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.ServiceAvailable (m, decision) = 
        x.Map (m, Configuration.add Decisions.ServiceAvailable (Decision.infer decision))

    [<CustomOperation (Decisions.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.UriTooLong (m, decision) = 
        x.Map (m, Configuration.add Decisions.UriTooLong (Decision.infer decision))

    (* Handlers *)

    // 200

    [<CustomOperation (Handlers.OK, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleOk (m, handler) = 
        x.Map (m, Configuration.add Handlers.OK (Handler.infer handler))

    [<CustomOperation (Handlers.Options, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleOptions (m, handler) = 
        x.Map (m, Configuration.add Handlers.Options (Handler.infer handler))

    [<CustomOperation (Handlers.Created, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleCreated (m, handler) = 
        x.Map (m, Configuration.add Handlers.Created (Handler.infer handler))

    [<CustomOperation (Handlers.Accepted, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleAccepted (m, handler) = 
        x.Map (m, Configuration.add Handlers.Accepted (Handler.infer handler))

    [<CustomOperation (Handlers.NoContent, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleNoContent (m, handler) = 
        x.Map (m, Configuration.add Handlers.NoContent (Handler.infer handler))

    // 300

    [<CustomOperation (Handlers.MovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleMovedPermanently (m, handler) = 
        x.Map (m, Configuration.add Handlers.MovedPermanently (Handler.infer handler))

    [<CustomOperation (Handlers.SeeOther, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleSeeOther (m, handler) = 
        x.Map (m, Configuration.add Handlers.SeeOther (Handler.infer handler))

    [<CustomOperation (Handlers.NotModified, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleNotModified (m, handler) = 
        x.Map (m, Configuration.add Handlers.NotModified (Handler.infer handler))

    [<CustomOperation (Handlers.MovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleMovedTemporarily (m, handler) = 
        x.Map (m, Configuration.add Handlers.MovedTemporarily (Handler.infer handler))

    [<CustomOperation (Handlers.MultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleMultipleRepresentations (m, handler) = 
        x.Map (m, Configuration.add Handlers.MultipleRepresentations (Handler.infer handler))
        
    // 400

    [<CustomOperation (Handlers.BadRequest, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleBadRequest (m, handler) = 
        x.Map (m, Configuration.add Handlers.BadRequest (Handler.infer handler))

    [<CustomOperation (Handlers.Unauthorized, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleUnauthorized (m, handler) = 
        x.Map (m, Configuration.add Handlers.Unauthorized (Handler.infer handler))

    [<CustomOperation (Handlers.Forbidden, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleForbidden (m, handler) = 
        x.Map (m, Configuration.add Handlers.Forbidden (Handler.infer handler))

    [<CustomOperation (Handlers.NotFound, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleNotFound (m, handler) = 
        x.Map (m, Configuration.add Handlers.NotFound (Handler.infer handler))

    [<CustomOperation (Handlers.MethodNotAllowed, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleMethodNotAllowed (m, handler) = 
        x.Map (m, Configuration.add Handlers.MethodNotAllowed (Handler.infer handler))

    [<CustomOperation (Handlers.NotAcceptable, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleNotAcceptable (m, handler) = 
        x.Map (m, Configuration.add Handlers.NotAcceptable (Handler.infer handler))

    [<CustomOperation (Handlers.Conflict, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleConflict (m, handler) = 
        x.Map (m, Configuration.add Handlers.Conflict (Handler.infer handler))

    [<CustomOperation (Handlers.Gone, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleGone (m, handler) = 
        x.Map (m, Configuration.add Handlers.Gone (Handler.infer handler))

    [<CustomOperation (Handlers.PreconditionFailed, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandlePreconditionFailed (m, handler) = 
        x.Map (m, Configuration.add Handlers.PreconditionFailed (Handler.infer handler))

    [<CustomOperation (Handlers.RequestEntityTooLarge, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleRequestEntityTooLarge (m, handler) = 
        x.Map (m, Configuration.add Handlers.RequestEntityTooLarge (Handler.infer handler))

    [<CustomOperation (Handlers.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleUriTooLong (m, handler) = 
        x.Map (m, Configuration.add Handlers.UriTooLong (Handler.infer handler))

    [<CustomOperation (Handlers.UnsupportedMediaType, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleUnsupportedMediaType (m, handler) = 
        x.Map (m, Configuration.add Handlers.UnsupportedMediaType (Handler.infer handler))

    [<CustomOperation (Handlers.UnprocessableEntity, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleUnprocessableEntity (m, handler) = 
        x.Map (m, Configuration.add Handlers.UnprocessableEntity (Handler.infer handler))

    // 500

    [<CustomOperation (Handlers.NotImplemented, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleNotImplemented (m, handler) = 
        x.Map (m, Configuration.add Handlers.NotImplemented (Handler.infer handler))

    [<CustomOperation (Handlers.UnknownMethod, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleUnknownMethod (m, handler) = 
        x.Map (m, Configuration.add Handlers.UnknownMethod (Handler.infer handler))
    
    [<CustomOperation (Handlers.ServiceUnavailable, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.HandleServiceUnavailable (m, handler) =
        x.Map (m, Configuration.add Handlers.ServiceUnavailable (Handler.infer handler))

    (* Properties *)

    [<CustomOperation (Properties.CharsetsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.CharsetsSupported (m, supported) = 
        x.Map (m, Configuration.add Properties.CharsetsSupported (Charsets.infer supported))

    [<CustomOperation (Properties.EncodingsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.EncodingsSupported (m, supported) = 
        x.Map (m, Configuration.add Properties.EncodingsSupported (ContentCodings.infer supported))

    [<CustomOperation (Properties.ETag, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.ETag (m, etag) = 
        x.Map (m, Configuration.add Properties.ETag (ETag.infer etag))

    [<CustomOperation (Properties.Expires, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.Expires (m, expires) = 
        x.Map (m, Configuration.add Properties.Expires (DateTime.infer expires))

    [<CustomOperation (Properties.LanguagesSupported, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.LanguagesSupported (m, supported) = 
        x.Map (m, Configuration.add Properties.LanguagesSupported (LanguageTags.infer supported))

    [<CustomOperation (Properties.LastModified, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.LastModified (m, modified) = 
        x.Map (m, Configuration.add Properties.LastModified (DateTime.infer modified))

    [<CustomOperation (Properties.Location, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.Location (m, location) = 
        x.Map (m, Configuration.add Properties.Location (UriReference.infer location))

    [<CustomOperation (Properties.MediaTypesSupported, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.MediaTypesSupported (m, supported) =
        x.Map (m, Configuration.add Properties.MediaTypesSupported (MediaTypes.infer supported))

    [<CustomOperation (Properties.MethodsKnown, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.MethodsKnown (m, known) = 
        x.Map (m, Configuration.add Properties.MethodsKnown (Methods.infer known))

    [<CustomOperation (Properties.MethodsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.MethodsSupported (m, supported) = 
        x.Map (m, Configuration.add Properties.MethodsSupported (Methods.infer supported))