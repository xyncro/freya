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
//----------------------------------------------------------------------------

[<AutoOpen>]
module Freya.Machine.Syntax

open System
open Freya.Core
open Freya.Types.Http
open Freya.Types.Http.Cors
open Freya.Types.Language
open Freya.Types.Uri

(* Custom Operations

   Custom syntax operators used in the FreyaMachine computation
   expression. Custom syntax operators are used heavily and are the
   configuration mechanism for configuring a machine resource. *)

type FreyaMachineBuilder with

    (* Actions *)

    [<CustomOperation (Actions.Delete, MaintainsVariableSpaceUsingBind = true)>]
    member x.DoDelete (monad, m) = 
        x.Set (monad, actionKeyPLens Actions.Delete, m)

    [<CustomOperation (Actions.Patch, MaintainsVariableSpaceUsingBind = true)>]
    member x.DoPatch (monad, m) = 
        x.Set (monad, actionKeyPLens Actions.Patch, m)

    [<CustomOperation (Actions.Post, MaintainsVariableSpaceUsingBind = true)>]
    member x.DoPost (monad, m) = 
        x.Set (monad, actionKeyPLens Actions.Post, m)

    [<CustomOperation (Actions.Put, MaintainsVariableSpaceUsingBind = true)>]
    member x.DoPut (monad, m) =
        x.Set (monad, actionKeyPLens Actions.Put, m)

    (* Configuration *)

    [<CustomOperation (Configuration.CharsetsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.CharsetsSupported (monad, charsets: Freya<Charset list>) = 
        x.Set (monad, configurationKeyPLens Configuration.CharsetsSupported, charsets)

    [<CustomOperation (Configuration.CorsHeadersExposed, MaintainsVariableSpaceUsingBind = true)>]
    member x.CorsHeadersExposed (monad, headers: Freya<string list>) = 
        x.Set (monad, configurationKeyPLens Configuration.CorsHeadersExposed, headers)

    [<CustomOperation (Configuration.CorsHeadersSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.CorsHeadersSupported (monad, headers: Freya<string list>) = 
        x.Set (monad, configurationKeyPLens Configuration.CorsHeadersSupported, headers)

    [<CustomOperation (Configuration.CorsMethodsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.CorsMethodsSupported (monad, methods: Freya<Method list>) = 
        x.Set (monad, configurationKeyPLens Configuration.CorsMethodsSupported, methods)

    [<CustomOperation (Configuration.CorsOriginsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.CorsOriginsSupported (monad, origins: Freya<AccessControlAllowOriginRange>) = 
        x.Set (monad, configurationKeyPLens Configuration.CorsOriginsSupported, origins)

    [<CustomOperation (Configuration.EncodingsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.EncodingsSupported (monad, encodings: Freya<ContentCoding list>) = 
        x.Set (monad, configurationKeyPLens Configuration.EncodingsSupported, encodings)

    [<CustomOperation (Configuration.ETag, MaintainsVariableSpaceUsingBind = true)>]
    member x.ETag (monad, etag: Freya<EntityTag>) = 
        x.Set (monad, configurationKeyPLens Configuration.ETag, etag)

    [<CustomOperation (Configuration.Expires, MaintainsVariableSpaceUsingBind = true)>]
    member x.Expires (monad, expires: Freya<DateTime>) = 
        x.Set (monad, configurationKeyPLens Configuration.Expires, expires)

    [<CustomOperation (Configuration.LanguagesSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.LanguagesSupported (monad, languages: Freya<LanguageTag list>) = 
        x.Set (monad, configurationKeyPLens Configuration.LanguagesSupported, languages)

    [<CustomOperation (Configuration.LastModified, MaintainsVariableSpaceUsingBind = true)>]
    member x.LastModified (monad, modified: Freya<DateTime>) = 
        x.Set (monad, configurationKeyPLens Configuration.LastModified, modified)

    [<CustomOperation (Configuration.Location, MaintainsVariableSpaceUsingBind = true)>]
    member x.Location (monad, location: Freya<UriReference>) = 
        x.Set (monad, configurationKeyPLens Configuration.Location, location)

    [<CustomOperation (Configuration.MediaTypesSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.MediaTypesSupported (monad, mediaTypes: Freya<MediaType list>) =
        x.Set (monad, configurationKeyPLens Configuration.MediaTypesSupported, mediaTypes)

    [<CustomOperation (Configuration.MethodsKnown, MaintainsVariableSpaceUsingBind = true)>]
    member x.MethodsKnown (monad, methods: Freya<Method list>) = 
        x.Set (monad, configurationKeyPLens Configuration.MethodsKnown, methods)

    [<CustomOperation (Configuration.MethodsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.MethodsSupported (monad, methods: Freya<Method list>) = 
        x.Set (monad, configurationKeyPLens Configuration.MethodsSupported, methods)

    (* Decisions *)

    [<CustomOperation (Decisions.Allowed, MaintainsVariableSpaceUsingBind = true)>]
    member x.Allowed (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.Allowed, m)

    [<CustomOperation (Decisions.AllowPostToGone, MaintainsVariableSpaceUsingBind = true)>]
    member x.AllowPostToGone (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.AllowPostToGone, m)

    [<CustomOperation (Decisions.AllowPostToMissing, MaintainsVariableSpaceUsingBind = true)>]
    member x.AllowPostToMissing (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.AllowPostToMissing, m)

    [<CustomOperation (Decisions.AllowPutToMissing, MaintainsVariableSpaceUsingBind = true)>]
    member x.AllowPutToMissing (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.AllowPutToMissing, m)

    [<CustomOperation (Decisions.Authorized, MaintainsVariableSpaceUsingBind = true)>]
    member x.Authorized (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.Authorized, m)

    [<CustomOperation (Decisions.CharsetsStrict, MaintainsVariableSpaceUsingBind = true)>]
    member x.CharsetsStrict (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.CharsetsStrict, m)

    [<CustomOperation (Decisions.Conflicts, MaintainsVariableSpaceUsingBind = true)>]
    member x.Conflicts (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.Conflicts, m)

    [<CustomOperation (Decisions.ContentTypeKnown, MaintainsVariableSpaceUsingBind = true)>]
    member x.ContentTypeKnown (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.ContentTypeKnown, m)

    [<CustomOperation (Decisions.ContentTypeValid, MaintainsVariableSpaceUsingBind = true)>]
    member x.ContentTypeValid (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.ContentTypeValid, m)

    [<CustomOperation (Decisions.Created, MaintainsVariableSpaceUsingBind = true)>]
    member x.Created (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.Created, m)

    [<CustomOperation (Decisions.Deleted, MaintainsVariableSpaceUsingBind = true)>]
    member x.Deleted (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.Deleted, m)

    [<CustomOperation (Decisions.EncodingsStrict, MaintainsVariableSpaceUsingBind = true)>]
    member x.EncodingsStrict (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.EncodingsStrict, m)

    [<CustomOperation (Decisions.EntityLengthValid, MaintainsVariableSpaceUsingBind = true)>]
    member x.EntityLengthValid (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.EntityLengthValid, m)

    [<CustomOperation (Decisions.Existed, MaintainsVariableSpaceUsingBind = true)>]
    member x.Existed (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.Existed, m)

    [<CustomOperation (Decisions.Exists, MaintainsVariableSpaceUsingBind = true)>]
    member x.Exists (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.Exists, m)

    [<CustomOperation (Decisions.LanguagesStrict, MaintainsVariableSpaceUsingBind = true)>]
    member x.LanguagesStrict (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.LanguagesStrict, m)

    [<CustomOperation (Decisions.Malformed, MaintainsVariableSpaceUsingBind = true)>]
    member x.Malformed (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.Malformed, m)

    [<CustomOperation (Decisions.MediaTypesStrict, MaintainsVariableSpaceUsingBind = true)>]
    member x.MediaTypesStrict (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.MediaTypesStrict, m)

    [<CustomOperation (Decisions.MovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
    member x.MovedPermanently (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.MovedPermanently, m)

    [<CustomOperation (Decisions.MovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
    member x.MovedTemporarily (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.MovedTemporarily, m)

    [<CustomOperation (Decisions.MultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
    member x.MultipleRepresentations (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.MultipleRepresentations, m)

    [<CustomOperation (Decisions.PostRedirect, MaintainsVariableSpaceUsingBind = true)>]
    member x.PostRedirect (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.PostRedirect, m)

    [<CustomOperation (Decisions.Processable, MaintainsVariableSpaceUsingBind = true)>]
    member x.Processable (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.Processable, m)

    [<CustomOperation (Decisions.PutToDifferentUri, MaintainsVariableSpaceUsingBind = true)>]
    member x.PutToDifferentUri (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.PutToDifferentUri, m)

    [<CustomOperation (Decisions.RespondWithEntity, MaintainsVariableSpaceUsingBind = true)>]
    member x.RespondWithEntity (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.RespondWithEntity, m)

    [<CustomOperation (Decisions.ServiceAvailable, MaintainsVariableSpaceUsingBind = true)>]
    member x.ServiceAvailable (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.ServiceAvailable, m)

    [<CustomOperation (Decisions.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
    member x.UriTooLong (monad, m) = 
        x.Set (monad, decisionKeyPLens Decisions.UriTooLong, m)

    (* Handlers *)

    // 200

    [<CustomOperation (Handlers.OK, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleOk (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.OK, m)

    [<CustomOperation (Handlers.Options, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleOptions (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.Options, m)

    [<CustomOperation (Handlers.Created, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleCreated (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.Created, m)

    [<CustomOperation (Handlers.Accepted, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleAccepted (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.Accepted, m)

    [<CustomOperation (Handlers.NoContent, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleNoContent (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.NoContent, m)

    // 300

    [<CustomOperation (Handlers.MovedPermanently, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleMovedPermanently (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.MovedPermanently, m)

    [<CustomOperation (Handlers.SeeOther, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleSeeOther (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.SeeOther, m)

    [<CustomOperation (Handlers.NotModified, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleNotModified (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.NotModified, m)

    [<CustomOperation (Handlers.MovedTemporarily, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleMovedTemporarily (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.MovedTemporarily, m)

    [<CustomOperation (Handlers.MultipleRepresentations, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleMultipleRepresentations (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.MultipleRepresentations, m)
        
    // 400

    [<CustomOperation (Handlers.BadRequest, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleBadRequest (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.BadRequest, m)

    [<CustomOperation (Handlers.Unauthorized, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleUnauthorized (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.Unauthorized, m)

    [<CustomOperation (Handlers.Forbidden, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleForbidden (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.Forbidden, m)

    [<CustomOperation (Handlers.NotFound, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleNotFound (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.NotFound, m)

    [<CustomOperation (Handlers.MethodNotAllowed, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleMethodNotAllowed (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.MethodNotAllowed, m)

    [<CustomOperation (Handlers.NotAcceptable, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleNotAcceptable (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.NotAcceptable, m)

    [<CustomOperation (Handlers.Conflict, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleConflict (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.Conflict, m)

    [<CustomOperation (Handlers.Gone, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleGone (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.Gone, m)

    [<CustomOperation (Handlers.PreconditionFailed, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandlePreconditionFailed (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.PreconditionFailed, m)

    [<CustomOperation (Handlers.RequestEntityTooLarge, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleRequestEntityTooLarge (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.RequestEntityTooLarge, m)

    [<CustomOperation (Handlers.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleUriTooLong (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.UriTooLong, m)

    [<CustomOperation (Handlers.UnsupportedMediaType, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleUnsupportedMediaType (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.UnsupportedMediaType, m)

    [<CustomOperation (Handlers.UnprocessableEntity, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleUnprocessableEntity (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.UnprocessableEntity, m)

    // 500

    [<CustomOperation (Handlers.NotImplemented, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleNotImplemented (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.NotImplemented, m)

    [<CustomOperation (Handlers.UnknownMethod, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleUnknownMethod (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.UnknownMethod, m)
    
    [<CustomOperation (Handlers.ServiceUnavailable, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleServiceUnavailable (monad, m) = 
        x.Set (monad, handlerKeyPLens Handlers.ServiceUnavailable, m)

    (* Utility *)

    [<CustomOperation ("including", MaintainsVariableSpaceUsingBind = true)>]
    member x.Including (monad, h) = 
        x.Combine (monad, h)