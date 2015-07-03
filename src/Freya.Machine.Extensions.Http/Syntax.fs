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

(* Helper Functions *)

let private setConfig<'a> key a =
    Lens.map FreyaMachineSpecification.ConfigurationLens (Configuration.set<'a> key a)

(* Custom Operations

   Custom syntax operators used in the FreyaMachine computation
   expression. Custom syntax operators are used heavily and are the
   configuration mechanism for configuring a machine resource. *)

type FreyaMachineBuilder with

    (* Actions *)

    [<CustomOperation (Actions.Delete, MaintainsVariableSpaceUsingBind = true)>]
    member x.DoDelete (m, delete: Freya<unit>) =
        x.Map (m, setConfig Actions.Delete delete)

    [<CustomOperation (Actions.Patch, MaintainsVariableSpaceUsingBind = true)>]
    member x.DoPatch (m, patch) = 
        x.Map (m, setConfig Actions.Patch patch)

    [<CustomOperation (Actions.Post, MaintainsVariableSpaceUsingBind = true)>]
    member x.DoPost (m, post) = 
        x.Map (m, setConfig Actions.Post post)

    [<CustomOperation (Actions.Put, MaintainsVariableSpaceUsingBind = true)>]
    member x.DoPut (m, put) =
        x.Map (m, setConfig Actions.Put put)

    (* Decisions *)

    [<CustomOperation (Decisions.Allowed, MaintainsVariableSpaceUsingBind = true)>]
    member x.Allowed (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.Allowed d)

    [<CustomOperation (Decisions.AllowPostToGone, MaintainsVariableSpaceUsingBind = true)>]
    member x.AllowPostToGone (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.AllowPostToGone d)

    [<CustomOperation (Decisions.AllowPostToMissing, MaintainsVariableSpaceUsingBind = true)>]
    member x.AllowPostToMissing (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.AllowPostToMissing d)

    [<CustomOperation (Decisions.AllowPutToMissing, MaintainsVariableSpaceUsingBind = true)>]
    member x.AllowPutToMissing (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.AllowPutToMissing d)

    [<CustomOperation (Decisions.Authorized, MaintainsVariableSpaceUsingBind = true)>]
    member x.Authorized (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.Authorized d)

    [<CustomOperation (Decisions.CharsetsStrict, MaintainsVariableSpaceUsingBind = true)>]
    member x.CharsetsStrict (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.CharsetsStrict d)

    [<CustomOperation (Decisions.Conflicts, MaintainsVariableSpaceUsingBind = true)>]
    member x.Conflicts (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.Conflicts d)

    [<CustomOperation (Decisions.ContentTypeKnown, MaintainsVariableSpaceUsingBind = true)>]
    member x.ContentTypeKnown (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.ContentTypeKnown d)

    [<CustomOperation (Decisions.ContentTypeValid, MaintainsVariableSpaceUsingBind = true)>]
    member x.ContentTypeValid (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.ContentTypeValid d)

    [<CustomOperation (Decisions.Created, MaintainsVariableSpaceUsingBind = true)>]
    member x.Created (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.Created d)

    [<CustomOperation (Decisions.Deleted, MaintainsVariableSpaceUsingBind = true)>]
    member x.Deleted (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.Deleted d)

    [<CustomOperation (Decisions.EncodingsStrict, MaintainsVariableSpaceUsingBind = true)>]
    member x.EncodingsStrict (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.EncodingsStrict d)

    [<CustomOperation (Decisions.EntityLengthValid, MaintainsVariableSpaceUsingBind = true)>]
    member x.EntityLengthValid (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.EntityLengthValid d)

    [<CustomOperation (Decisions.Existed, MaintainsVariableSpaceUsingBind = true)>]
    member x.Existed (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.Existed d)

    [<CustomOperation (Decisions.Exists, MaintainsVariableSpaceUsingBind = true)>]
    member x.Exists (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.Exists d)

    [<CustomOperation (Decisions.LanguagesStrict, MaintainsVariableSpaceUsingBind = true)>]
    member x.LanguagesStrict (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.LanguagesStrict d)

    [<CustomOperation (Decisions.Malformed, MaintainsVariableSpaceUsingBind = true)>]
    member x.Malformed (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.Malformed d)

    [<CustomOperation (Decisions.MediaTypesStrict, MaintainsVariableSpaceUsingBind = true)>]
    member x.MediaTypesStrict (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.MediaTypesStrict d)

    [<CustomOperation (Decisions.MovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
    member x.MovedPermanently (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.MovedPermanently d)

    [<CustomOperation (Decisions.MovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
    member x.MovedTemporarily (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.MovedTemporarily d)

    [<CustomOperation (Decisions.MultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
    member x.MultipleRepresentations (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.MultipleRepresentations d)

    [<CustomOperation (Decisions.PostRedirect, MaintainsVariableSpaceUsingBind = true)>]
    member x.PostRedirect (m, d: Freya<bool>) =
        x.Map (m, setConfig Decisions.PostRedirect d)

    [<CustomOperation (Decisions.Processable, MaintainsVariableSpaceUsingBind = true)>]
    member x.Processable (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.Processable d)

    [<CustomOperation (Decisions.PutToDifferentUri, MaintainsVariableSpaceUsingBind = true)>]
    member x.PutToDifferentUri (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.PutToDifferentUri d)

    [<CustomOperation (Decisions.RespondWithEntity, MaintainsVariableSpaceUsingBind = true)>]
    member x.RespondWithEntity (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.RespondWithEntity d)

    [<CustomOperation (Decisions.ServiceAvailable, MaintainsVariableSpaceUsingBind = true)>]
    member x.ServiceAvailable (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.ServiceAvailable d)

    [<CustomOperation (Decisions.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
    member x.UriTooLong (m, d: Freya<bool>) = 
        x.Map (m, setConfig Decisions.UriTooLong d)

    (* Handlers *)

    // 200

    [<CustomOperation (Handlers.OK, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleOk (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.OK h)

    [<CustomOperation (Handlers.Options, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleOptions (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.Options h)

    [<CustomOperation (Handlers.Created, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleCreated (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.Created h)

    [<CustomOperation (Handlers.Accepted, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleAccepted (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.Accepted h)

    [<CustomOperation (Handlers.NoContent, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleNoContent (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.NoContent h)

    // 300

    [<CustomOperation (Handlers.MovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleMovedPermanently (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.MovedPermanently h)

    [<CustomOperation (Handlers.SeeOther, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleSeeOther (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.SeeOther h)

    [<CustomOperation (Handlers.NotModified, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleNotModified (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.NotModified h)

    [<CustomOperation (Handlers.MovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleMovedTemporarily (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.MovedTemporarily h)

    [<CustomOperation (Handlers.MultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleMultipleRepresentations (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.MultipleRepresentations h)
        
    // 400

    [<CustomOperation (Handlers.BadRequest, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleBadRequest (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.BadRequest h)

    [<CustomOperation (Handlers.Unauthorized, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleUnauthorized (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.Unauthorized h)

    [<CustomOperation (Handlers.Forbidden, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleForbidden (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.Forbidden h)

    [<CustomOperation (Handlers.NotFound, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleNotFound (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.NotFound h)

    [<CustomOperation (Handlers.MethodNotAllowed, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleMethodNotAllowed (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.MethodNotAllowed h)

    [<CustomOperation (Handlers.NotAcceptable, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleNotAcceptable (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.NotAcceptable h)

    [<CustomOperation (Handlers.Conflict, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleConflict (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.Conflict h)

    [<CustomOperation (Handlers.Gone, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleGone (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.Gone h)

    [<CustomOperation (Handlers.PreconditionFailed, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandlePreconditionFailed (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.PreconditionFailed h)

    [<CustomOperation (Handlers.RequestEntityTooLarge, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleRequestEntityTooLarge (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.RequestEntityTooLarge h)

    [<CustomOperation (Handlers.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleUriTooLong (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.UriTooLong h)

    [<CustomOperation (Handlers.UnsupportedMediaType, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleUnsupportedMediaType (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.UnsupportedMediaType h)

    [<CustomOperation (Handlers.UnprocessableEntity, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleUnprocessableEntity (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.UnprocessableEntity h)

    // 500

    [<CustomOperation (Handlers.NotImplemented, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleNotImplemented (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.NotImplemented h)

    [<CustomOperation (Handlers.UnknownMethod, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleUnknownMethod (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.UnknownMethod h)
    
    [<CustomOperation (Handlers.ServiceUnavailable, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleServiceUnavailable (m, h: Specification -> Freya<Representation>) = 
        x.Map (m, setConfig Handlers.ServiceUnavailable h)

    (* Properties *)

    [<CustomOperation (Properties.CharsetsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.CharsetsSupported (m, charsets: Freya<Charset list>) = 
        x.Map (m, setConfig Properties.CharsetsSupported charsets)

    [<CustomOperation (Properties.EncodingsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.EncodingsSupported (m, encodings: Freya<ContentCoding list>) = 
        x.Map (m, setConfig Properties.EncodingsSupported encodings)

    [<CustomOperation (Properties.ETag, MaintainsVariableSpaceUsingBind = true)>]
    member x.ETag (m, etag: Freya<EntityTag>) = 
        x.Map (m, setConfig Properties.ETag etag)

    [<CustomOperation (Properties.Expires, MaintainsVariableSpaceUsingBind = true)>]
    member x.Expires (m, expires: Freya<DateTime>) = 
        x.Map (m, setConfig Properties.Expires expires)

    [<CustomOperation (Properties.LanguagesSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.LanguagesSupported (m, languages: Freya<LanguageTag list>) = 
        x.Map (m, setConfig Properties.LanguagesSupported languages)

    [<CustomOperation (Properties.LastModified, MaintainsVariableSpaceUsingBind = true)>]
    member x.LastModified (m, modified: Freya<DateTime>) = 
        x.Map (m, setConfig Properties.LastModified modified)

    [<CustomOperation (Properties.Location, MaintainsVariableSpaceUsingBind = true)>]
    member x.Location (m, location: Freya<UriReference>) = 
        x.Map (m, setConfig Properties.Location location)

    [<CustomOperation (Properties.MediaTypesSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.MediaTypesSupported (m, mediaTypes: Freya<MediaType list>) =
        x.Map (m, setConfig Properties.MediaTypesSupported mediaTypes)

    [<CustomOperation (Properties.MethodsKnown, MaintainsVariableSpaceUsingBind = true)>]
    member x.MethodsKnown (m, methods: Freya<Method list>) = 
        x.Map (m, setConfig Properties.MethodsKnown methods)

    [<CustomOperation (Properties.MethodsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.MethodsSupported (m, methods: Freya<Method list>) = 
        x.Map (m, setConfig Properties.MethodsSupported methods)