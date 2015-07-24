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
open Freya.Core.Operators
open Freya.Lenses.Http
open Freya.Machine
open Freya.Machine.Operators

(* Decisions *)

let private systemDecision f =
    Some (Compile (fun config -> 
        Compiled (Binary (f config), unconfigurable)))

let private userDecision key def =
    Some (Compile (Configuration.tryGet key
        >> Option.map (fun x -> Compiled (Binary x, configured))
        >> Option.orElse (Compiled (Binary (Freya.init def), unconfigured))))

let private charsetNegotiable config =
    ContentNegotiation.Charset.negotiable
        (!?. Request.Headers.AcceptCharset_)
        (Configuration.tryGetOrElse Properties.CharsetsSupported Defaults.charsetsSupported config)

let private charsetRequested _ =
    ContentNegotiation.Charset.requested 
        (!?. Request.Headers.AcceptCharset_)

let private encodingNegotiable config =
    ContentNegotiation.Encoding.negotiable
        (!?. Request.Headers.AcceptEncoding_)
        (Configuration.tryGetOrElse Properties.EncodingsSupported Defaults.encodingsSupported config)

let private encodingRequested _ =
    ContentNegotiation.Encoding.requested 
        (!?. Request.Headers.AcceptEncoding_)

let private ifMatchAny _ =
    CacheControl.IfMatch.any 
        (!?. Request.Headers.IfMatch_)

let private ifMatchExistsForMissing _ =
    CacheControl.IfMatch.requested 
        (!?. Request.Headers.IfMatch_)

let private ifMatchRequested _ =
    CacheControl.IfMatch.requested 
        (!?. Request.Headers.IfMatch_)

let private ifModifiedSinceModified config =
    CacheControl.IfModifiedSince.modified
        (!?. Request.Headers.IfModifiedSince_)
        (Configuration.tryGetOrElse Properties.LastModified Defaults.lastModified config)

let private ifModifiedSinceRequested _ =
    CacheControl.IfModifiedSince.requested
        (!?. Request.Headers.IfModifiedSince_)

let private ifModifiedSinceValid _ =
    CacheControl.IfModifiedSince.valid
        (!?. Request.Headers.IfModifiedSince_)

let private ifNoneMatchAny _ =
    CacheControl.IfNoneMatch.any
        (!?. Request.Headers.IfNoneMatch_)

let private ifNoneMatchRequested _ =
    CacheControl.IfNoneMatch.requested
        (!?. Request.Headers.IfNoneMatch_)

let private ifUnmodifiedSinceModified config =
    CacheControl.IfUnmodifiedSince.unmodified
        (!?. Request.Headers.IfUnmodifiedSince_)
        (Configuration.tryGetOrElse Properties.LastModified Defaults.lastModified config)

let private ifUnmodifiedSinceRequested _ =
    CacheControl.IfUnmodifiedSince.requested
        (!?. Request.Headers.IfUnmodifiedSince_)

let private ifUnmodifiedSinceValid _ =
    CacheControl.IfUnmodifiedSince.valid
        (!?. Request.Headers.IfUnmodifiedSince_)

let private languageNegotiable config =
    ContentNegotiation.Language.negotiable
        (!?. Request.Headers.AcceptLanguage_)
        (Configuration.tryGetOrElse Properties.LanguagesSupported Defaults.languagesSupported config)

let private languageRequested _ =
    ContentNegotiation.Language.requested
        (!?. Request.Headers.AcceptLanguage_)

let private mediaTypeNegotiable config =
    ContentNegotiation.MediaType.negotiable
        (!?. Request.Headers.Accept_)
        (Configuration.tryGetOrElse Properties.MediaTypesSupported Defaults.mediaTypesSupported config)

let private mediaTypeRequested _ =
    ContentNegotiation.MediaType.requested
        (!?. Request.Headers.Accept_)

let private methodDelete _ =
    Method.delete
        (!. Request.Method_)

let private methodGetOrHead _ =
    Method.getOrHead
        (!. Request.Method_)

let private methodKnown config =
    Method.known
        (!. Request.Method_)
        (Configuration.tryGetOrElse Properties.MethodsKnown Defaults.methodsKnown config)

let private methodOptions _ =
    Method.options
        (!. Request.Method_)

let private methodPatch _ =
    Method.patch
        (!. Request.Method_)

let private methodPostToExisting _ =
    Method.post
        (!. Request.Method_)

let private methodPostToGone _ =
    Method.post
        (!. Request.Method_)

let private methodPostToMissing _ =
    Method.post
        (!. Request.Method_)

let private methodPut _ =
    Method.put
        (!. Request.Method_)

let private methodPutToExisting _ =
    Method.put
        (!. Request.Method_)

let private methodSupported config =
    Method.supported
        (!. Request.Method_)
        (Configuration.tryGetOrElse Properties.MethodsSupported Defaults.methodsSupported config)

(* Graph *)

let operations =
    [ Operation Decisions.Allowed                      =.        userDecision Decisions.Allowed true
      Operation Decisions.Authorized                   =.        userDecision Decisions.Authorized true
      Operation Decisions.AllowPostToGone              =.        userDecision Decisions.AllowPostToGone false
      Operation Decisions.AllowPostToMissing           =.        userDecision Decisions.AllowPostToMissing true
      Operation Decisions.AllowPutToMissing            =.        userDecision Decisions.AllowPutToMissing true
      Operation Decisions.CharsetsStrict               =.        userDecision Decisions.CharsetsStrict false
      Operation Decisions.Conflicts                    =.        userDecision Decisions.Conflicts false
      Operation Decisions.ContentTypeKnown             =.        userDecision Decisions.ContentTypeKnown true
      Operation Decisions.ContentTypeValid             =.        userDecision Decisions.ContentTypeValid true
      Operation Decisions.Created                      =.        userDecision Decisions.Created true
      Operation Decisions.Deleted                      =.        userDecision Decisions.Deleted true
      Operation Decisions.EncodingsStrict              =.        userDecision Decisions.EncodingsStrict false
      Operation Decisions.EntityLengthValid            =.        userDecision Decisions.EntityLengthValid true
      Operation Decisions.Existed                      =.        userDecision Decisions.Existed false
      Operation Decisions.Exists                       =.        userDecision Decisions.Exists true
      Operation Decisions.LanguagesStrict              =.        userDecision Decisions.LanguagesStrict false
      Operation Decisions.Malformed                    =.        userDecision Decisions.Malformed false
      Operation Decisions.MediaTypesStrict             =.        userDecision Decisions.MediaTypesStrict true
      Operation Decisions.MovedPermanently             =.        userDecision Decisions.MovedPermanently false
      Operation Decisions.MovedTemporarily             =.        userDecision Decisions.MovedTemporarily false
      Operation Decisions.MultipleRepresentations      =.        userDecision Decisions.MultipleRepresentations false
      Operation Decisions.PostRedirect                 =.        userDecision Decisions.PostRedirect false
      Operation Decisions.Processable                  =.        userDecision Decisions.Processable true
      Operation Decisions.PutToDifferentUri            =.        userDecision Decisions.PutToDifferentUri false
      Operation Decisions.RespondWithEntity            =.        userDecision Decisions.RespondWithEntity true
      Operation Decisions.ServiceAvailable             =.        userDecision Decisions.ServiceAvailable true
      Operation Decisions.UriTooLong                   =.        userDecision Decisions.UriTooLong false
        
      Operation Decisions.CharsetNegotiable            =.        systemDecision charsetNegotiable
      Operation Decisions.CharsetRequested             =.        systemDecision charsetRequested
      Operation Decisions.EncodingNegotiable           =.        systemDecision encodingNegotiable
      Operation Decisions.EncodingRequested            =.        systemDecision encodingRequested
      Operation Decisions.IfMatchAny                   =.        systemDecision ifMatchAny
      Operation Decisions.IfMatchExistsForMissing      =.        systemDecision ifMatchExistsForMissing
      Operation Decisions.IfMatchRequested             =.        systemDecision ifMatchRequested
      Operation Decisions.IfModifiedSinceModified      =.        systemDecision ifModifiedSinceModified
      Operation Decisions.IfModifiedSinceRequested     =.        systemDecision ifModifiedSinceRequested
      Operation Decisions.IfModifiedSinceValid         =.        systemDecision ifModifiedSinceValid
      Operation Decisions.IfNoneMatchAny               =.        systemDecision ifNoneMatchAny
      Operation Decisions.IfNoneMatchRequested         =.        systemDecision ifNoneMatchRequested
      Operation Decisions.IfUnmodifiedSinceModified    =.        systemDecision ifUnmodifiedSinceModified
      Operation Decisions.IfUnmodifiedSinceRequested   =.        systemDecision ifUnmodifiedSinceRequested
      Operation Decisions.IfUnmodifiedSinceValid       =.        systemDecision ifUnmodifiedSinceValid
      Operation Decisions.LanguageNegotiable           =.        systemDecision languageNegotiable
      Operation Decisions.LanguageRequested            =.        systemDecision languageRequested
      Operation Decisions.MediaTypeNegotiable          =.        systemDecision mediaTypeNegotiable
      Operation Decisions.MediaTypeRequested           =.        systemDecision mediaTypeRequested
      Operation Decisions.MethodDelete                 =.        systemDecision methodDelete
      Operation Decisions.MethodGetOrHead              =.        systemDecision methodGetOrHead
      Operation Decisions.MethodKnown                  =.        systemDecision methodKnown
      Operation Decisions.MethodOptions                =.        systemDecision methodOptions
      Operation Decisions.MethodPatch                  =.        systemDecision methodPatch
      Operation Decisions.MethodPostToExisting         =.        systemDecision methodPostToExisting
      Operation Decisions.MethodPostToGone             =.        systemDecision methodPostToGone
      Operation Decisions.MethodPostToMissing          =.        systemDecision methodPostToMissing
      Operation Decisions.MethodPut                    =.        systemDecision methodPut
      Operation Decisions.MethodPutToExisting          =.        systemDecision methodPutToExisting
      Operation Decisions.MethodSupported              =.        systemDecision methodSupported

      Operation Decisions.ETagMatchesIf                =.        systemDecision (fun _ -> Freya.init true)
      Operation Decisions.ETagMatchesIfNone            =.        systemDecision (fun _ -> Freya.init true)

      Operation Decisions.CharsetNegotiable            >+        Operation Decisions.EncodingRequested           
      Operation Decisions.CharsetNegotiable            >-        Operation Decisions.CharsetsStrict
      Operation Decisions.CharsetRequested             >+        Operation Decisions.CharsetNegotiable           
      Operation Decisions.CharsetRequested             >-        Operation Decisions.EncodingRequested
      Operation Decisions.EncodingNegotiable           >+        Operation Decisions.Processable                 
      Operation Decisions.EncodingNegotiable           >-        Operation Decisions.EncodingsStrict
      Operation Decisions.EncodingRequested            >+        Operation Decisions.EncodingNegotiable          
      Operation Decisions.EncodingRequested            >-        Operation Decisions.Processable
      Operation Decisions.IfMatchAny                   >+        Operation Decisions.IfUnmodifiedSinceRequested  
      Operation Decisions.IfMatchAny                   >-        Operation Decisions.ETagMatchesIf
      Operation Decisions.IfMatchExistsForMissing      >+        Operation Operations.PreconditionFailed         
      Operation Decisions.IfMatchExistsForMissing      >-        Operation Decisions.MethodPut
      Operation Decisions.IfMatchRequested             >+        Operation Decisions.IfMatchAny                  
      Operation Decisions.IfMatchRequested             >-        Operation Decisions.IfUnmodifiedSinceRequested
      Operation Decisions.IfModifiedSinceModified      >+        Operation Decisions.MethodDelete                
      Operation Decisions.IfModifiedSinceModified      >-        Operation Operations.NotModified
      Operation Decisions.IfModifiedSinceRequested     >+        Operation Decisions.IfModifiedSinceValid        
      Operation Decisions.IfModifiedSinceRequested     >-        Operation Decisions.MethodDelete
      Operation Decisions.IfModifiedSinceValid         >+        Operation Decisions.IfModifiedSinceModified     
      Operation Decisions.IfModifiedSinceValid         >-        Operation Decisions.MethodDelete
      Operation Decisions.IfNoneMatchAny               >+        Operation Decisions.MethodGetOrHead             
      Operation Decisions.IfNoneMatchAny               >-        Operation Decisions.ETagMatchesIfNone
      Operation Decisions.IfNoneMatchRequested         >+        Operation Decisions.IfNoneMatchAny              
      Operation Decisions.IfNoneMatchRequested         >-        Operation Decisions.IfModifiedSinceRequested
      Operation Decisions.IfUnmodifiedSinceModified    >+        Operation Decisions.IfNoneMatchRequested        
      Operation Decisions.IfUnmodifiedSinceModified    >-        Operation Operations.PreconditionFailed
      Operation Decisions.IfUnmodifiedSinceRequested   >+        Operation Decisions.IfUnmodifiedSinceValid      
      Operation Decisions.IfUnmodifiedSinceRequested   >-        Operation Decisions.IfNoneMatchRequested
      Operation Decisions.IfUnmodifiedSinceValid       >+        Operation Decisions.IfUnmodifiedSinceModified   
      Operation Decisions.IfUnmodifiedSinceValid       >-        Operation Decisions.IfNoneMatchRequested
      Operation Decisions.LanguageNegotiable           >+        Operation Decisions.CharsetRequested            
      Operation Decisions.LanguageNegotiable           >-        Operation Decisions.LanguagesStrict
      Operation Decisions.LanguageRequested            >+        Operation Decisions.LanguageNegotiable          
      Operation Decisions.LanguageRequested            >-        Operation Decisions.CharsetRequested
      Operation Decisions.MediaTypeNegotiable          >+        Operation Decisions.LanguageRequested           
      Operation Decisions.MediaTypeNegotiable          >-        Operation Decisions.MediaTypesStrict
      Operation Decisions.MediaTypeRequested           >+        Operation Decisions.MediaTypeNegotiable         
      Operation Decisions.MediaTypeRequested           >-        Operation Decisions.LanguageRequested
      Operation Decisions.MethodDelete                 >+        Operation Actions.Delete                        
      Operation Decisions.MethodDelete                 >-        Operation Decisions.MethodPatch
      Operation Decisions.MethodGetOrHead              >+        Operation Operations.NotModified                
      Operation Decisions.MethodGetOrHead              >-        Operation Operations.PreconditionFailed
      Operation Decisions.MethodKnown                  >+        Operation Decisions.UriTooLong                  
      Operation Decisions.MethodKnown                  >-        Operation Operations.UnknownMethod
      Operation Decisions.MethodOptions                >+        Operation Operations.Options                    
      Operation Decisions.MethodOptions                >-        Operation Decisions.MediaTypeRequested
      Operation Decisions.MethodPatch                  >+        Operation Actions.Patch                         
      Operation Decisions.MethodPatch                  >-        Operation Decisions.MethodPostToExisting
      Operation Decisions.MethodPostToExisting         >+        Operation Actions.Post                          
      Operation Decisions.MethodPostToExisting         >-        Operation Decisions.MethodPutToExisting
      Operation Decisions.MethodPostToGone             >+        Operation Decisions.AllowPostToGone             
      Operation Decisions.MethodPostToGone             >-        Operation Operations.Gone
      Operation Decisions.MethodPostToMissing          >+        Operation Decisions.AllowPostToMissing          
      Operation Decisions.MethodPostToMissing          >-        Operation Operations.NotFound
      Operation Decisions.MethodPut                    >+        Operation Decisions.PutToDifferentUri           
      Operation Decisions.MethodPut                    >-        Operation Decisions.Existed
      Operation Decisions.MethodPutToExisting          >+        Operation Decisions.Conflicts                   
      Operation Decisions.MethodPutToExisting          >-        Operation Decisions.MultipleRepresentations
      Operation Decisions.MethodSupported              >+        Operation Decisions.Malformed                   
      Operation Decisions.MethodSupported              >-        Operation Operations.MethodNotAllowed

      Operation Decisions.ETagMatchesIf                >+        Operation Decisions.IfUnmodifiedSinceRequested  
      Operation Decisions.ETagMatchesIf                >-        Operation Operations.PreconditionFailed
      Operation Decisions.ETagMatchesIfNone            >+        Operation Decisions.MethodGetOrHead             
      Operation Decisions.ETagMatchesIfNone            >-        Operation Decisions.IfModifiedSinceRequested

      Operation Decisions.Allowed                      >+        Operation Decisions.ContentTypeValid
      Operation Decisions.Allowed                      >-        Operation Operations.Forbidden
      Operation Decisions.Authorized                   >+        Operation Decisions.Allowed
      Operation Decisions.Authorized                   >-        Operation Operations.Unauthorized
      Operation Decisions.AllowPostToGone              >+        Operation Actions.Post
      Operation Decisions.AllowPostToGone              >-        Operation Operations.Gone
      Operation Decisions.AllowPostToMissing           >+        Operation Actions.Post
      Operation Decisions.AllowPostToMissing           >-        Operation Operations.NotFound
      Operation Decisions.AllowPutToMissing            >+        Operation Decisions.Conflicts
      Operation Decisions.AllowPutToMissing            >-        Operation Operations.NotImplemented
      Operation Decisions.CharsetsStrict               >+        Operation Operations.NotAcceptable
      Operation Decisions.CharsetsStrict               >-        Operation Decisions.EncodingRequested
      Operation Decisions.Conflicts                    >+        Operation Operations.Conflict
      Operation Decisions.Conflicts                    >-        Operation Actions.Put
      Operation Decisions.ContentTypeKnown             >+        Operation Decisions.EntityLengthValid
      Operation Decisions.ContentTypeKnown             >-        Operation Operations.UnsupportedMediaType
      Operation Decisions.ContentTypeValid             >+        Operation Decisions.ContentTypeKnown
      Operation Decisions.ContentTypeValid             >-        Operation Operations.NotImplemented
      Operation Decisions.Created                      >+        Operation Operations.Created
      Operation Decisions.Created                      >-        Operation Decisions.RespondWithEntity
      Operation Decisions.Deleted                      >+        Operation Decisions.RespondWithEntity
      Operation Decisions.Deleted                      >-        Operation Operations.Accepted
      Operation Decisions.EncodingsStrict              >+        Operation Operations.NotAcceptable
      Operation Decisions.EncodingsStrict              >-        Operation Decisions.Processable
      Operation Decisions.EntityLengthValid            >+        Operation Decisions.MethodOptions
      Operation Decisions.EntityLengthValid            >-        Operation Operations.RequestEntityTooLarge
      Operation Decisions.Existed                      >+        Operation Decisions.MovedPermanently
      Operation Decisions.Existed                      >-        Operation Decisions.MethodPostToMissing
      Operation Decisions.Exists                       >+        Operation Decisions.IfMatchRequested
      Operation Decisions.Exists                       >-        Operation Decisions.IfMatchExistsForMissing
      Operation Decisions.LanguagesStrict              >+        Operation Operations.NotAcceptable
      Operation Decisions.LanguagesStrict              >-        Operation Decisions.CharsetRequested
      Operation Decisions.Malformed                    >+        Operation Operations.BadRequest
      Operation Decisions.Malformed                    >-        Operation Decisions.Authorized
      Operation Decisions.MediaTypesStrict             >+        Operation Operations.NotAcceptable
      Operation Decisions.MediaTypesStrict             >-        Operation Decisions.LanguageRequested
      Operation Decisions.MovedPermanently             >+        Operation Operations.MovedPermanently
      Operation Decisions.MovedPermanently             >-        Operation Decisions.MovedTemporarily
      Operation Decisions.MovedTemporarily             >+        Operation Operations.MovedTemporarily
      Operation Decisions.MovedTemporarily             >-        Operation Decisions.MethodPostToGone
      Operation Decisions.MultipleRepresentations      >+        Operation Operations.MultipleRepresentations
      Operation Decisions.MultipleRepresentations      >-        Operation Operations.OK
      Operation Decisions.PostRedirect                 >+        Operation Operations.SeeOther
      Operation Decisions.PostRedirect                 >-        Operation Decisions.Created
      Operation Decisions.Processable                  >+        Operation Decisions.Exists
      Operation Decisions.Processable                  >-        Operation Operations.UnprocessableEntity
      Operation Decisions.PutToDifferentUri            >+        Operation Operations.MovedPermanently
      Operation Decisions.PutToDifferentUri            >-        Operation Decisions.AllowPutToMissing
      Operation Decisions.RespondWithEntity            >+        Operation Decisions.MultipleRepresentations
      Operation Decisions.RespondWithEntity            >-        Operation Operations.NoContent
      Operation Decisions.ServiceAvailable             >+        Operation Decisions.MethodKnown
      Operation Decisions.ServiceAvailable             >-        Operation Operations.ServiceUnavailable
      Operation Decisions.UriTooLong                   >+        Operation Operations.UriTooLong
      Operation Decisions.UriTooLong                   >-        Operation Decisions.MethodSupported ]