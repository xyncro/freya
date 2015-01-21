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

[<RequireQualifiedAccess>]
module internal Freya.Machine.Extensions.Http.Decisions

open Freya.Core
open Freya.Machine
open Freya.Machine.Operators
open Freya.Types.Http

(* Decisions *)

let systemDecision f =
    Binary (fun config -> unconfigurable, f config)

let userDecision key def =
    Binary (tryGetConfiguration key
            >> Option.map (fun x -> configured, x)
            >> Option.orElse (unconfigured, Freya.init def))

let charsetNegotiable config =
    ContentNegotiation.Charset.negotiable
        (getPLM Request.Headers.acceptCharset)
        (tryGetConfiguration Configuration.CharsetsSupported config |> Option.orElse Defaults.charsetsSupported)

let charsetRequested _ =
    ContentNegotiation.Charset.requested 
        (getPLM Request.Headers.acceptCharset)

let encodingNegotiable config =
    ContentNegotiation.Encoding.negotiable
        (getPLM Request.Headers.acceptEncoding)
        (tryGetConfiguration Configuration.EncodingsSupported config |> Option.orElse Defaults.encodingsSupported)

let encodingRequested _ =
    ContentNegotiation.Encoding.requested 
        (getPLM Request.Headers.acceptEncoding)

let ifMatchAny _ =
    CacheControl.IfMatch.any 
        (getPLM Request.Headers.ifMatch)

let ifMatchExistsForMissing _ =
    CacheControl.IfMatch.requested 
        (getPLM Request.Headers.ifMatch)

let ifMatchRequested _ =
    CacheControl.IfMatch.requested 
        (getPLM Request.Headers.ifMatch)

let ifModifiedSinceModified config =
    CacheControl.IfModifiedSince.modified
        (getPLM Request.Headers.ifModifiedSince)
        (tryGetConfiguration Configuration.LastModified config |> Option.orElse Defaults.lastModified)

let ifModifiedSinceRequested _ =
    CacheControl.IfModifiedSince.requested
        (getPLM Request.Headers.ifModifiedSince)

let ifModifiedSinceValid _ =
    CacheControl.IfModifiedSince.valid
        (getPLM Request.Headers.ifModifiedSince)

let ifNoneMatchAny _ =
    CacheControl.IfNoneMatch.any
        (getPLM Request.Headers.ifNoneMatch)

let ifNoneMatchRequested _ =
    CacheControl.IfNoneMatch.requested
        (getPLM Request.Headers.ifNoneMatch)

let ifUnmodifiedSinceModified config =
    CacheControl.IfUnmodifiedSince.unmodified
        (getPLM Request.Headers.ifUnmodifiedSince)
        (tryGetConfiguration Configuration.LastModified config |> Option.orElse Defaults.lastModified)

let ifUnmodifiedSinceRequested _ =
    CacheControl.IfUnmodifiedSince.requested
        (getPLM Request.Headers.ifUnmodifiedSince)

let ifUnmodifiedSinceValid _ =
    CacheControl.IfUnmodifiedSince.valid
        (getPLM Request.Headers.ifUnmodifiedSince)

let languageNegotiable config =
    ContentNegotiation.Language.negotiable
        (getPLM Request.Headers.acceptLanguage)
        (tryGetConfiguration Configuration.LanguagesSupported config |> Option.orElse Defaults.languagesSupported)

let languageRequested _ =
    ContentNegotiation.Language.requested
        (getPLM Request.Headers.acceptLanguage)

let mediaTypeNegotiable config =
    ContentNegotiation.MediaType.negotiable
        (getPLM Request.Headers.accept)
        (tryGetConfiguration Configuration.MediaTypesSupported config |> Option.orElse Defaults.mediaTypesSupported)

let mediaTypeRequested _ =
    ContentNegotiation.MediaType.requested
        (getPLM Request.Headers.accept)

let methodDelete _ =
    Method.delete
        (getLM Request.meth)

let methodGetOrHead _ =
    Method.getOrHead
        (getLM Request.meth)

let methodKnown config =
    Method.known
        (getLM Request.meth)
        (tryGetConfiguration Configuration.MethodsKnown config |> Option.orElse Defaults.methodsKnown)

let methodOptions _ =
    Method.options
        (getLM Request.meth)

let methodPatch _ =
    Method.patch
        (getLM Request.meth)

let methodPostToExisting _ =
    Method.post
        (getLM Request.meth)

let methodPostToGone _ =
    Method.post
        (getLM Request.meth)

let methodPostToMissing _ =
    Method.post
        (getLM Request.meth)

let methodPut _ =
    Method.put
        (getLM Request.meth)

let methodPutToExisting _ =
    Method.put
        (getLM Request.meth)

let methodSupported config =
    Method.supported
        (getLM Request.meth)
        (tryGetConfiguration Configuration.MethodsSupported config |> Option.orElse Defaults.methodsSupported)

(* Graph *)

let operations =
    [ Ref Decisions.Allowed                            =.        userDecision Decisions.Allowed true
      Ref Decisions.Authorized                         =.        userDecision Decisions.Authorized true
      Ref Decisions.AllowPostToGone                    =.        userDecision Decisions.AllowPostToGone false
      Ref Decisions.AllowPostToMissing                 =.        userDecision Decisions.AllowPostToMissing true
      Ref Decisions.AllowPutToMissing                  =.        userDecision Decisions.AllowPutToMissing true
      Ref Decisions.CharsetsStrict                     =.        userDecision Decisions.CharsetsStrict false
      Ref Decisions.Conflicts                          =.        userDecision Decisions.Conflicts false
      Ref Decisions.ContentTypeKnown                   =.        userDecision Decisions.ContentTypeKnown true
      Ref Decisions.ContentTypeValid                   =.        userDecision Decisions.ContentTypeValid true
      Ref Decisions.Created                            =.        userDecision Decisions.Created true
      Ref Decisions.Deleted                            =.        userDecision Decisions.Deleted true
      Ref Decisions.EncodingsStrict                    =.        userDecision Decisions.EncodingsStrict false
      Ref Decisions.EntityLengthValid                  =.        userDecision Decisions.EntityLengthValid true
      Ref Decisions.Existed                            =.        userDecision Decisions.Existed false
      Ref Decisions.Exists                             =.        userDecision Decisions.Exists true
      Ref Decisions.LanguagesStrict                    =.        userDecision Decisions.LanguagesStrict false
      Ref Decisions.Malformed                          =.        userDecision Decisions.Malformed false
      Ref Decisions.MediaTypesStrict                   =.        userDecision Decisions.MediaTypesStrict true
      Ref Decisions.MovedPermanently                   =.        userDecision Decisions.MovedPermanently false
      Ref Decisions.MovedTemporarily                   =.        userDecision Decisions.MovedTemporarily false
      Ref Decisions.MultipleRepresentations            =.        userDecision Decisions.MultipleRepresentations false
      Ref Decisions.PostRedirect                       =.        userDecision Decisions.PostRedirect false
      Ref Decisions.Processable                        =.        userDecision Decisions.Processable true
      Ref Decisions.PutToDifferentUri                  =.        userDecision Decisions.PutToDifferentUri false
      Ref Decisions.RespondWithEntity                  =.        userDecision Decisions.RespondWithEntity true
      Ref Decisions.ServiceAvailable                   =.        userDecision Decisions.ServiceAvailable true
      Ref Decisions.UriTooLong                         =.        userDecision Decisions.UriTooLong false
        
      Ref Decisions.CharsetNegotiable                  =.        systemDecision charsetNegotiable
      Ref Decisions.CharsetRequested                   =.        systemDecision charsetRequested
      Ref Decisions.EncodingNegotiable                 =.        systemDecision encodingNegotiable
      Ref Decisions.EncodingRequested                  =.        systemDecision encodingRequested
      Ref Decisions.IfMatchAny                         =.        systemDecision ifMatchAny
      Ref Decisions.IfMatchExistsForMissing            =.        systemDecision ifMatchExistsForMissing
      Ref Decisions.IfMatchRequested                   =.        systemDecision ifMatchRequested
      Ref Decisions.IfModifiedSinceModified            =.        systemDecision ifModifiedSinceModified
      Ref Decisions.IfModifiedSinceRequested           =.        systemDecision ifModifiedSinceRequested
      Ref Decisions.IfModifiedSinceValid               =.        systemDecision ifModifiedSinceValid
      Ref Decisions.IfNoneMatchAny                     =.        systemDecision ifNoneMatchAny
      Ref Decisions.IfNoneMatchRequested               =.        systemDecision ifNoneMatchRequested
      Ref Decisions.IfUnmodifiedSinceModified          =.        systemDecision ifUnmodifiedSinceModified
      Ref Decisions.IfUnmodifiedSinceRequested         =.        systemDecision ifUnmodifiedSinceRequested
      Ref Decisions.IfUnmodifiedSinceValid             =.        systemDecision ifUnmodifiedSinceValid
      Ref Decisions.LanguageNegotiable                 =.        systemDecision languageNegotiable
      Ref Decisions.LanguageRequested                  =.        systemDecision languageRequested
      Ref Decisions.MediaTypeNegotiable                =.        systemDecision mediaTypeNegotiable
      Ref Decisions.MediaTypeRequested                 =.        systemDecision mediaTypeRequested
      Ref Decisions.MethodDelete                       =.        systemDecision methodDelete
      Ref Decisions.MethodGetOrHead                    =.        systemDecision methodGetOrHead
      Ref Decisions.MethodKnown                        =.        systemDecision methodKnown
      Ref Decisions.MethodOptions                      =.        systemDecision methodOptions
      Ref Decisions.MethodPatch                        =.        systemDecision methodPatch
      Ref Decisions.MethodPostToExisting               =.        systemDecision methodPostToExisting
      Ref Decisions.MethodPostToGone                   =.        systemDecision methodPostToGone
      Ref Decisions.MethodPostToMissing                =.        systemDecision methodPostToMissing
      Ref Decisions.MethodPut                          =.        systemDecision methodPut
      Ref Decisions.MethodPutToExisting                =.        systemDecision methodPutToExisting
      Ref Decisions.MethodSupported                    =.        systemDecision methodSupported

      Ref Decisions.ETagMatchesIf                      =.        Binary (fun _ -> unconfigured, Freya.init true)
      Ref Decisions.ETagMatchesIfNone                  =.        Binary (fun _ -> unconfigured, Freya.init true)

      Ref Decisions.CharsetNegotiable                  >+        Ref Decisions.EncodingRequested           
      Ref Decisions.CharsetNegotiable                  >-        Ref Decisions.CharsetsStrict
      Ref Decisions.CharsetRequested                   >+        Ref Decisions.CharsetNegotiable           
      Ref Decisions.CharsetRequested                   >-        Ref Decisions.EncodingRequested
      Ref Decisions.EncodingNegotiable                 >+        Ref Decisions.Processable                 
      Ref Decisions.EncodingNegotiable                 >-        Ref Decisions.EncodingsStrict
      Ref Decisions.EncodingRequested                  >+        Ref Decisions.EncodingNegotiable          
      Ref Decisions.EncodingRequested                  >-        Ref Decisions.Processable
      Ref Decisions.IfMatchAny                         >+        Ref Decisions.IfUnmodifiedSinceRequested  
      Ref Decisions.IfMatchAny                         >-        Ref Decisions.ETagMatchesIf
      Ref Decisions.IfMatchExistsForMissing            >+        Ref Operations.PreconditionFailed         
      Ref Decisions.IfMatchExistsForMissing            >-        Ref Decisions.MethodPut
      Ref Decisions.IfMatchRequested                   >+        Ref Decisions.IfMatchAny                  
      Ref Decisions.IfMatchRequested                   >-        Ref Decisions.IfUnmodifiedSinceRequested
      Ref Decisions.IfModifiedSinceModified            >+        Ref Decisions.MethodDelete                
      Ref Decisions.IfModifiedSinceModified            >-        Ref Operations.NotModified
      Ref Decisions.IfModifiedSinceRequested           >+        Ref Decisions.IfModifiedSinceValid        
      Ref Decisions.IfModifiedSinceRequested           >-        Ref Decisions.MethodDelete
      Ref Decisions.IfModifiedSinceValid               >+        Ref Decisions.IfModifiedSinceModified     
      Ref Decisions.IfModifiedSinceValid               >-        Ref Decisions.MethodDelete
      Ref Decisions.IfNoneMatchAny                     >+        Ref Decisions.MethodGetOrHead             
      Ref Decisions.IfNoneMatchAny                     >-        Ref Decisions.ETagMatchesIfNone
      Ref Decisions.IfNoneMatchRequested               >+        Ref Decisions.IfNoneMatchAny              
      Ref Decisions.IfNoneMatchRequested               >-        Ref Decisions.IfModifiedSinceRequested
      Ref Decisions.IfUnmodifiedSinceModified          >+        Ref Decisions.IfNoneMatchRequested        
      Ref Decisions.IfUnmodifiedSinceModified          >-        Ref Operations.PreconditionFailed
      Ref Decisions.IfUnmodifiedSinceRequested         >+        Ref Decisions.IfUnmodifiedSinceValid      
      Ref Decisions.IfUnmodifiedSinceRequested         >-        Ref Decisions.IfNoneMatchRequested
      Ref Decisions.IfUnmodifiedSinceValid             >+        Ref Decisions.IfUnmodifiedSinceModified   
      Ref Decisions.IfUnmodifiedSinceValid             >-        Ref Decisions.IfNoneMatchRequested
      Ref Decisions.LanguageNegotiable                 >+        Ref Decisions.CharsetRequested            
      Ref Decisions.LanguageNegotiable                 >-        Ref Decisions.LanguagesStrict
      Ref Decisions.LanguageRequested                  >+        Ref Decisions.LanguageNegotiable          
      Ref Decisions.LanguageRequested                  >-        Ref Decisions.CharsetRequested
      Ref Decisions.MediaTypeNegotiable                >+        Ref Decisions.LanguageRequested           
      Ref Decisions.MediaTypeNegotiable                >-        Ref Decisions.MediaTypesStrict
      Ref Decisions.MediaTypeRequested                 >+        Ref Decisions.MediaTypeNegotiable         
      Ref Decisions.MediaTypeRequested                 >-        Ref Decisions.LanguageRequested
      Ref Decisions.MethodDelete                       >+        Ref Actions.Delete                        
      Ref Decisions.MethodDelete                       >-        Ref Decisions.MethodPatch
      Ref Decisions.MethodGetOrHead                    >+        Ref Operations.NotModified                
      Ref Decisions.MethodGetOrHead                    >-        Ref Operations.PreconditionFailed
      Ref Decisions.MethodKnown                        >+        Ref Decisions.UriTooLong                  
      Ref Decisions.MethodKnown                        >-        Ref Operations.UnknownMethod
      Ref Decisions.MethodOptions                      >+        Ref Operations.Options                    
      Ref Decisions.MethodOptions                      >-        Ref Decisions.MediaTypeRequested
      Ref Decisions.MethodPatch                        >+        Ref Actions.Patch                         
      Ref Decisions.MethodPatch                        >-        Ref Decisions.MethodPostToExisting
      Ref Decisions.MethodPostToExisting               >+        Ref Actions.Post                          
      Ref Decisions.MethodPostToExisting               >-        Ref Decisions.MethodPutToExisting
      Ref Decisions.MethodPostToGone                   >+        Ref Decisions.AllowPostToGone             
      Ref Decisions.MethodPostToGone                   >-        Ref Operations.Gone
      Ref Decisions.MethodPostToMissing                >+        Ref Decisions.AllowPostToMissing          
      Ref Decisions.MethodPostToMissing                >-        Ref Operations.NotFound
      Ref Decisions.MethodPut                          >+        Ref Decisions.PutToDifferentUri           
      Ref Decisions.MethodPut                          >-        Ref Decisions.Existed
      Ref Decisions.MethodPutToExisting                >+        Ref Decisions.Conflicts                   
      Ref Decisions.MethodPutToExisting                >-        Ref Decisions.MultipleRepresentations
      Ref Decisions.MethodSupported                    >+        Ref Decisions.Malformed                   
      Ref Decisions.MethodSupported                    >-        Ref Operations.MethodNotAllowed

      Ref Decisions.ETagMatchesIf                      >+        Ref Decisions.IfUnmodifiedSinceRequested  
      Ref Decisions.ETagMatchesIf                      >-        Ref Operations.PreconditionFailed
      Ref Decisions.ETagMatchesIfNone                  >+        Ref Decisions.MethodGetOrHead             
      Ref Decisions.ETagMatchesIfNone                  >-        Ref Decisions.IfModifiedSinceRequested

      Ref Decisions.Allowed                            >+        Ref Decisions.ContentTypeValid
      Ref Decisions.Allowed                            >-        Ref Operations.Forbidden
      Ref Decisions.Authorized                         >+        Ref Decisions.Allowed
      Ref Decisions.Authorized                         >-        Ref Operations.Unauthorized
      Ref Decisions.AllowPostToGone                    >+        Ref Actions.Post
      Ref Decisions.AllowPostToGone                    >-        Ref Operations.Gone
      Ref Decisions.AllowPostToMissing                 >+        Ref Actions.Post
      Ref Decisions.AllowPostToMissing                 >-        Ref Operations.NotFound
      Ref Decisions.AllowPutToMissing                  >+        Ref Decisions.Conflicts
      Ref Decisions.AllowPutToMissing                  >-        Ref Operations.NotImplemented
      Ref Decisions.CharsetsStrict                     >+        Ref Operations.NotAcceptable
      Ref Decisions.CharsetsStrict                     >-        Ref Decisions.EncodingRequested
      Ref Decisions.Conflicts                          >+        Ref Operations.Conflict
      Ref Decisions.Conflicts                          >-        Ref Actions.Put
      Ref Decisions.ContentTypeKnown                   >+        Ref Decisions.EntityLengthValid
      Ref Decisions.ContentTypeKnown                   >-        Ref Operations.UnsupportedMediaType
      Ref Decisions.ContentTypeValid                   >+        Ref Decisions.ContentTypeKnown
      Ref Decisions.ContentTypeValid                   >-        Ref Operations.NotImplemented
      Ref Decisions.Created                            >+        Ref Operations.Created
      Ref Decisions.Created                            >-        Ref Decisions.RespondWithEntity
      Ref Decisions.Deleted                            >+        Ref Decisions.RespondWithEntity
      Ref Decisions.Deleted                            >-        Ref Operations.Accepted
      Ref Decisions.EncodingsStrict                    >+        Ref Operations.NotAcceptable
      Ref Decisions.EncodingsStrict                    >-        Ref Decisions.Processable
      Ref Decisions.EntityLengthValid                  >+        Ref Decisions.MethodOptions
      Ref Decisions.EntityLengthValid                  >-        Ref Operations.RequestEntityTooLarge
      Ref Decisions.Existed                            >+        Ref Decisions.MovedPermanently
      Ref Decisions.Existed                            >-        Ref Decisions.MethodPostToMissing
      Ref Decisions.Exists                             >+        Ref Decisions.IfMatchRequested
      Ref Decisions.Exists                             >-        Ref Decisions.IfMatchExistsForMissing
      Ref Decisions.LanguagesStrict                    >+        Ref Operations.NotAcceptable
      Ref Decisions.LanguagesStrict                    >-        Ref Decisions.CharsetRequested
      Ref Decisions.Malformed                          >+        Ref Operations.BadRequest
      Ref Decisions.Malformed                          >-        Ref Decisions.Authorized
      Ref Decisions.MediaTypesStrict                   >+        Ref Operations.NotAcceptable
      Ref Decisions.MediaTypesStrict                   >-        Ref Decisions.LanguageRequested
      Ref Decisions.MovedPermanently                   >+        Ref Operations.MovedPermanently
      Ref Decisions.MovedPermanently                   >-        Ref Decisions.MovedTemporarily
      Ref Decisions.MovedTemporarily                   >+        Ref Operations.MovedTemporarily
      Ref Decisions.MovedTemporarily                   >-        Ref Decisions.MethodPostToGone
      Ref Decisions.MultipleRepresentations            >+        Ref Operations.MultipleRepresentations
      Ref Decisions.MultipleRepresentations            >-        Ref Operations.OK
      Ref Decisions.PostRedirect                       >+        Ref Operations.SeeOther
      Ref Decisions.PostRedirect                       >-        Ref Decisions.Created
      Ref Decisions.Processable                        >+        Ref Decisions.Exists
      Ref Decisions.Processable                        >-        Ref Operations.UnprocessableEntity
      Ref Decisions.PutToDifferentUri                  >+        Ref Operations.MovedPermanently
      Ref Decisions.PutToDifferentUri                  >-        Ref Decisions.AllowPutToMissing
      Ref Decisions.RespondWithEntity                  >+        Ref Decisions.MultipleRepresentations
      Ref Decisions.RespondWithEntity                  >-        Ref Operations.NoContent
      Ref Decisions.ServiceAvailable                   >+        Ref Decisions.MethodKnown
      Ref Decisions.ServiceAvailable                   >-        Ref Operations.ServiceUnavailable
      Ref Decisions.UriTooLong                         >+        Ref Operations.UriTooLong
      Ref Decisions.UriTooLong                         >-        Ref Decisions.MethodSupported ]