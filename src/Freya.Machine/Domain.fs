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
module internal Freya.Machine.Domain

open System
open Freya.Core
open Freya.Core.Operators
open Freya.Types.Cors
open Freya.Types.Http
open Freya.Types.Language

(* Cache Control

   Logic for negotiating decisions based on Cache Control,
   given suitably configured (optional) definitions of what cache control
   is defined as part of the Machine Definition.

   Unlike some other "machine-type" frameworks, all configuration here
   is dynamic not static, and so will be evaluated at runtime, allowing
   for dynamic support based on the specific resource in question. *)

[<AutoOpen>]
module CacheControl =

    (* If-Match *)

    [<RequireQualifiedAccess>]
    module IfMatch =

        (* Request *)

        let private ifMatch =
            getPLM Request.Headers.ifMatch

        (* Decisions *)

        [<RequireQualifiedAccess>]
        module Decision =

            let requested : FreyaMachineDecision =
                    Option.isSome 
                <!> ifMatch

            let any : FreyaMachineDecision =
                    (=) (Some (IfMatch IfMatchChoice.Any)) 
                <!> ifMatch

    (* If-Modified-Since *)

    [<RequireQualifiedAccess>]
    module IfModifiedSince =

        (* Configuration *)

        let private lastModified =
            configurationKey Configuration.LastModified

        (* Request *)

        let private ifModifiedSince =
            getPLM Request.Headers.ifModifiedSince

        (* Decisions *)

        [<RequireQualifiedAccess>]
        module Decision =

            let requested : FreyaMachineDecision =
                    Option.isSome 
                <!> ifModifiedSince

            let valid : FreyaMachineDecision =
                    (function | Some (IfModifiedSince x) -> x < DateTime.UtcNow
                              | _ -> false)
                <!> ifModifiedSince

            let modified : FreyaMachineDecision =
                    (fun x y -> (function | Some lm, Some (IfModifiedSince ms) -> lm > ms
                                          | _ -> false) (x, y))
                <!> lastModified
                <*> ifModifiedSince

    (* If-None-Match *)

    [<RequireQualifiedAccess>]
    module IfNoneMatch =

        (* Request *)

        let private ifNoneMatch =
            getPLM Request.Headers.ifNoneMatch

        (* Decisions *)

        [<RequireQualifiedAccess>]
        module Decision =

            let requested : FreyaMachineDecision =
                    Option.isSome 
                <!> ifNoneMatch

            let any : FreyaMachineDecision =
                    (=) (Some (IfNoneMatch IfNoneMatchChoice.Any)) 
                <!> ifNoneMatch

    (* If-Unmodified-Since *)

    [<RequireQualifiedAccess>]
    module IfUnmodifiedSince =

        (* Configuration *)

        let private lastModified =
            configurationKey Configuration.LastModified

        (* Request *)

        let private ifUnmodifiedSince =
            getPLM Request.Headers.ifUnmodifiedSince

        (* Decisions *)

        [<RequireQualifiedAccess>]
        module Decision =

            let requested : FreyaMachineDecision =
                    Option.isSome 
                <!> ifUnmodifiedSince

            let valid : FreyaMachineDecision =
                    (function | Some (IfUnmodifiedSince x) -> x < DateTime.UtcNow
                              | _ -> false)
                <!> ifUnmodifiedSince

            let unmodified : FreyaMachineDecision =
                    (fun x y -> (function | Some lm, Some (IfUnmodifiedSince us) -> lm < us
                                          | _ -> true) (x, y))
                <!> lastModified
                <*> ifUnmodifiedSince

(* Content Negotiation

   Taken from RFC 7231, Section 5.3
   [http://tools.ietf.org/html/rfc7231#section-5.3]

   Logic for negotiating decisions based on Content Negotiation,
   given suitably configured (optional) definitions of what content
   is supported as part of the Machine Definition. 

   Unlike some other "machine-type" frameworks, all configuration here
   is dynamic not static, and so will be evaluated at runtime, allowing
   for dynamic support based on the specific resource in question. *)

[<AutoOpen>]
module ContentNegotiation =

    (* Charset *)

    [<RequireQualifiedAccess>]
    module internal Charset =

        (* Configuration *)

        let private charsetsSupported =
            configurationKey Configuration.CharsetsSupported

        (* Defaults *)

        let private defaultCharsetsSupported =
            [ Charset.Iso88591 ]

        (* Request *)

        let acceptCharset =
            getPLM Request.Headers.acceptCharset

        (* Derived *)

        let private charsetsSupported' =
                (function | Some x -> x
                          | _ -> defaultCharsetsSupported)
            <!> charsetsSupported

        (* Negotiation *)

        [<RequireQualifiedAccess>]
        module Negotiation =

            let private max (Charset s) =
                function | { AcceptableCharset.Charset = CharsetRange.Charset (Charset s') } when s == s' -> Some 0
                         | { Charset = CharsetRange.Any } -> Some 1
                         | _ -> None

            let private map requested =
                List.map (fun (x: Charset) ->
                    x, List.chooseMaxBy (max x) requested)

            let private sort =
                List.sortBy (fun (x, y) ->
                    (function | Some { AcceptableCharset.Weight = Some weight } -> 1. - weight
                              | _ -> 0.) y)

            let private choose =
                List.choose (fun (x, y) ->
                    (function | Some { AcceptableCharset.Weight = Some weight } when weight > 0. -> Some x
                              | Some { AcceptableCharset.Weight = None } -> Some x
                              | _ -> None) y)

            let private run requested =
                   map requested 
                >> sort
                >> choose

            let negotiate supported =
                function | Some (AcceptCharset x) -> Negotiated (run x supported)
                         | _ -> Free

            let negotiated =
                    negotiate
                <!> charsetsSupported'
                <*> acceptCharset

        (* Decisions *)

        [<RequireQualifiedAccess>]
        module Decision =

            let requested : FreyaMachineDecision =
                    Option.isSome 
                <!> getPLM Request.Headers.acceptCharset

            let negotiable : FreyaMachineDecision =
                    (function | Negotiated x when List.isEmpty x = false -> true
                              | _ -> false)
                <!> Negotiation.negotiated

    (* Encoding *)

    [<RequireQualifiedAccess>]
    module Encoding =

        // TODO: Better Content-Coding Negotiation - proper support of identity, etc.

        (* Configuration *)

        let private encodingsSupported =
            configurationKey Configuration.EncodingsSupported

        (* Defaults *)

        let private defaultEncodingsSupported =
            List.empty<ContentCoding>

        (* Request *)

        let private acceptEncoding =
            getPLM Request.Headers.acceptEncoding

        (* Derived *)

        let private encodingsSupported' =
                (function | Some x -> x
                          | _ -> defaultEncodingsSupported)
            <!> encodingsSupported

        (* Negotiation *)

        [<RequireQualifiedAccess>]
        module Negotiation =

            let private max (ContentCoding c) =
                function | { AcceptableEncoding.Encoding = EncodingRange.Coding (ContentCoding c') } when c == c' -> Some 0
                         | { Encoding = EncodingRange.Any } -> Some 1
                         | _ -> None

            let private map requested =
                List.map (fun (x: ContentCoding) ->
                    x, List.chooseMaxBy (max x) requested)

            let private sort =
                List.sortBy (fun (x, y) ->
                    (function | Some { AcceptableEncoding.Weight = Some weight } -> 1. - weight
                              | _ -> 0.) y)

            let private choose =
                List.choose (fun (x, y) ->
                    (function | Some { AcceptableEncoding.Weight = Some weight } when weight > 0. -> Some x
                              | Some { AcceptableEncoding.Weight = None } -> Some x
                              | _ -> None) y)

            let private run requested =
                   map requested 
                >> sort
                >> choose

            let negotiate supported =
                function | Some (AcceptEncoding x) -> Negotiated (run x supported)
                         | _ -> Free

            let negotiated =
                    negotiate 
                <!> encodingsSupported'
                <*> acceptEncoding

        (* Decisions *)

        [<RequireQualifiedAccess>]
        module Decision =

            let requested : FreyaMachineDecision =
                    Option.isSome 
                <!> acceptEncoding

            let negotiable : FreyaMachineDecision =
                    (function | Negotiated x when List.isEmpty x = false -> true
                              | _ -> false)
                <!> Negotiation.negotiated

    (* Language *)

    [<RequireQualifiedAccess>]
    module Language =

        (* Note: This is intended to approximate the semantics
           of Basic Filtering as specified in Section 3.3.1 of RFC 4647.

           See [http://tools.ietf.org/html/rfc4647#section-3.3.1] *)

        (* Configuration *)

        let private languagesSupported =
            configurationKey Configuration.LanguagesSupported

        (* Defaults *)

        let private defaultLanguagesSupported =
            List.empty<LanguageTag>

        (* Request *)

        let private acceptLanguage =
            getPLM Request.Headers.acceptLanguage

        (* Derived *)

        let private languagesSupported' =
                (function | Some x -> x
                          | _ -> defaultLanguagesSupported)
            <!> languagesSupported

        (* Negotiation *)

        [<RequireQualifiedAccess>]
        module Negotiation =

            let private reify tag =
                let language, extensions =
                    (function | Language (language, Some extensions) -> [ language ], extensions
                              | Language (language, _) -> [ language ], []) tag.Language
                let script =
                    (function | Some (Script script) -> [ script ]
                              | _ -> []) tag.Script
                let region =
                    (function | Some (Region region) -> [ region ]
                              | _ -> []) tag.Region
                let variant =
                    (function | Variant variant -> variant) tag.Variant

                List.concat [
                    language
                    extensions
                    script
                    region
                    variant ]

            let private eq tag =
                Seq.zip (reify tag) >> Seq.forall ((<||) (==))

            let private sort =
                List.sortBy (fun (x: AcceptableLanguage) -> 
                    (function | Some x -> 1. - x
                              | _ -> 0. ) x.Weight)

            let private filter =
                List.filter (fun (x: AcceptableLanguage) ->
                    (function | Some 0. -> false
                              | _ -> true) x.Weight)

            let private map supported =
                List.map (fun (x: AcceptableLanguage) ->
                    (function | Range x -> List.filter (flip eq x) supported
                              | Any -> supported) x.Language)
    
            let private run supported =
                   sort
                >> filter
                >> map supported
                >> Seq.concat
                >> Seq.distinct
                >> Seq.toList

            let negotiate supported =
                function | Some (AcceptLanguage x) -> Negotiated (run supported x)
                         | _ -> Free

            let negotiated =
                    negotiate 
                <!> languagesSupported'
                <*> acceptLanguage

        (* Decisions *)

        [<RequireQualifiedAccess>]
        module Decision =

            let requested : FreyaMachineDecision =
                    Option.isSome 
                <!> acceptLanguage

            let negotiable : FreyaMachineDecision =
                    (function | Negotiated x when List.isEmpty x = false -> true
                              | _ -> false)
                <!> Negotiation.negotiated

    (* MediaType *)

    [<RequireQualifiedAccess>]
    module MediaType =

        (* Configuration *)

        let private mediaTypesSupported =
            configurationKey Configuration.MediaTypesSupported

        (* Defaults *)

        let private defaultMediaTypesSupported =
            List.empty<MediaType>

        (* Request *)

        let private accept =
            getPLM Request.Headers.accept

        (* Derived *)

        let private mediaTypesSupported' =
                (function | Some x -> x
                          | _ -> defaultMediaTypesSupported)
            <!> mediaTypesSupported

        (* Negotiation *)

        [<RequireQualifiedAccess>]
        module Negotiation =

            let private max (MediaType (Type t, SubType s, _)) =
                function | { MediaRange = Closed (Type t', SubType s', _) } when t == t' && s == s' -> Some 0
                         | { MediaRange = MediaRange.Partial (Type t', _) } when t == t' -> Some 1
                         | { MediaRange = Open _ } -> Some 2
                         | _ -> None

            let private map requested =
                List.map (fun (x: MediaType) ->
                    x, List.chooseMaxBy (max x) requested)

            let private sort =
                List.sortBy (fun (x, y) ->
                    (function | Some { Parameters = Some { Weight = weight } } -> 1. - weight
                              | _ -> 0.) y)

            let private choose =
                List.choose (fun (x, y) ->
                    (function | Some { Parameters = Some { Weight = weight } } when weight > 0. -> Some x
                              | Some { Parameters = None } -> Some x
                              | _ -> None) y)

            let private run requested =
                   map requested 
                >> sort
                >> choose

            let negotiate supported =
                function | Some (Accept x) -> Negotiated (run x supported)
                         | _ -> Free

            let negotiated =
                    negotiate 
                <!> mediaTypesSupported'
                <*> accept

        (* Decisions *)

        [<RequireQualifiedAccess>]
        module Decision =

            let requested : FreyaMachineDecision =
                    Option.isSome 
                <!> accept

            let negotiable : FreyaMachineDecision =
                    (function | Negotiated x when List.isEmpty x = false -> true
                              | _ -> false)
                <!> Negotiation.negotiated

(* Cross Origin Resource Sharing

   Logic for negotiating decisions based on CORS (Cross-Origin Resource Sharing),
   properly supporting per-resource responses based on genuinely allowable requests,
   available methods outside of the simple set, etc. *)

[<RequireQualifiedAccess>]
module CrossOrigin =

    (* Abbreviations *)

    module Req = Request.Headers
    module Res = Response.Headers

    (* Configuration *)

    let private corsHeadersExposed =
        configurationKey Configuration.CorsHeadersExposed

    let private corsHeadersSupported =
        configurationKey Configuration.CorsHeadersSupported

    let private corsMethodsSupported =
        configurationKey Configuration.CorsMethodsSupported

    let private corsOriginsSupported =
        configurationKey Configuration.CorsOriginsSupported

    (* Request *)

    let private accessControlRequestHeaders =
        getPLM Request.Headers.accessControlRequestHeaders

    let private accessControlRequestMethod =
        getPLM Request.Headers.accessControlRequestMethod

    let private meth =
        getLM Request.meth

    let private origin =
        getPLM Request.Headers.origin

    (* Derived *)

    let private accessControlRequestHeaders' =
            (function | Some (AccessControlRequestHeaders x) -> x
                      | _ -> [])
        <!> accessControlRequestHeaders

    let private accessControlRequestMethod' =
            (Option.map (fun (AccessControlRequestMethod x) -> x) >> Option.get)
        <!> accessControlRequestMethod

    let private corsHeadersExposed' =
            (function | Some x -> x
                      | _ -> [])
        <!> corsHeadersExposed

    let private corsHeadersSupported' =
            (function | Some x -> x
                      | _ -> [])
        <!> corsHeadersSupported

    let private corsMethodsSupported' =
            (function | Some x -> x
                      | _ -> [])
        <!> corsMethodsSupported

    let private origin' =
            (Option.map (fun (Origin x) -> x) >> Option.get) 
        <!> origin

    (* Decisions *)

    [<RequireQualifiedAccess>]
    module Decision =

        let enabled : FreyaMachineDecision =
                Option.isSome
            <!> corsOriginsSupported

        let origin : FreyaMachineDecision =
                (fun origin origins ->
                    match origin, origins with
                    | Some _, 
                      Some (AccessControlAllowOriginRange.Any) -> true
                    | Some (Origin (OriginListOrNull.Origins (x :: []))), 
                      Some (Origins (OriginListOrNull.Origins xs)) -> List.exists ((=) x) xs
                    | _ -> false)
            <!> origin   
            <*> corsOriginsSupported

        let options : FreyaMachineDecision =
                (=) OPTIONS 
            <!> meth

        let preflight : FreyaMachineDecision =
                Option.isSome
            <!> accessControlRequestMethod

    (* Operations *)

    [<RequireQualifiedAccess>]
    module Operation =

        let private corsHeadersAllowed =
                (fun headers supported ->
                    match List.forall (fun x -> List.exists ((=) x) supported) headers with
                    | true -> headers
                    | _ -> [])
            <!> accessControlRequestHeaders'
            <*> corsHeadersSupported'

        let private corsMethodsAllowed =
                (fun meth supported ->
                    match List.exists ((=) meth) supported with
                    | true -> [ meth ]
                    | _ -> [])
            <!> accessControlRequestMethod'
            <*> corsMethodsSupported'

        let private accessControlAllowMethods =
                setPLM Res.accessControlAllowMethods
            =<< (AccessControlAllowMethods <!> corsMethodsAllowed)

        let private accessControlExposeHeaders =
                setPLM Res.accessControlExposeHeaders
            =<< (AccessControlExposeHeaders <!> corsHeadersExposed')

        let private accessControlAllowHeaders =
                setPLM Res.accessControlAllowHeaders
            =<< (AccessControlAllowHeaders <!> corsHeadersAllowed)

        let private accessControlAllowOrigin =
                setPLM Res.accessControlAllowOrigin 
            =<< ((Origins >> AccessControlAllowOrigin) <!> origin')

        let actual =
            accessControlExposeHeaders

        let origin =
            accessControlAllowOrigin

        let preflight =
                accessControlAllowMethods 
             *> accessControlAllowHeaders

(* Method *)

[<RequireQualifiedAccess>]
module Method =

    (* Configuration *)

    let private methodsKnown =
        configurationKey Configuration.MethodsKnown

    let private methodsSupported =
        configurationKey Configuration.MethodsSupported

    (* Defaults *)

    let private defaultMethodsKnown =
        [ DELETE
          HEAD
          GET
          OPTIONS
          PATCH
          POST
          PUT
          TRACE ]

    let private defaultMethodsSupported =
        [ GET
          HEAD ]
    
    (* Request *)

    let private meth =
        getLM Request.meth

    (* Derived *)

    let private methodsKnown' =
            (function | Some x -> x
                      | _ -> defaultMethodsKnown) 
        <!> methodsKnown

    let internal methodsSupported' =
            (function | Some x -> x
                      | _ -> defaultMethodsSupported)
        <!> methodsSupported

    (* Decisions *)

    [<RequireQualifiedAccess>]
    module Decision =

        let known : FreyaMachineDecision =
                (fun x -> List.exists (fun x' -> x = x'))
            <!> meth
            <*> methodsKnown'

        let supported : FreyaMachineDecision =
                (fun x -> List.exists (fun x' -> x = x'))
            <!> meth
            <*> methodsSupported'

        let delete : FreyaMachineDecision =
                (=) DELETE 
            <!> meth

        let getOrHead : FreyaMachineDecision =
                flip Set.contains (set [ GET; HEAD ])
            <!> meth

        let options : FreyaMachineDecision =
                (=) OPTIONS 
            <!> meth

        let patch : FreyaMachineDecision =
                (=) PATCH 
            <!> meth

        let post : FreyaMachineDecision =
                (=) POST 
            <!> meth

        let put : FreyaMachineDecision =
                (=) PUT 
            <!> meth

(* HTTP *)

[<RequireQualifiedAccess>]
module Http =

    // TODO: Content-Location, Cache-Control (??), Vary
    // TODO: Testing of new header setting!

    let private allow =
            (fun x -> setPLM Response.Headers.allow (Allow x))
        =<< Method.methodsSupported'

    let private date =
        setPLM Response.Headers.date (Date.Date DateTime.UtcNow)

    let private eTag =
            (function | Some x -> setPLM Response.Headers.eTag (ETag x)
                      | _ -> returnM ())
        =<< configurationKey Configuration.ETag

    let private expires =
            (function | Some x -> setPLM Response.Headers.expires (Expires x)
                      | _ -> returnM ())
        =<< configurationKey Configuration.Expires

    let private lastModified =
            (function | Some x -> setPLM Response.Headers.lastModified (LastModified x)
                      | _ -> returnM ())
        =<< configurationKey Configuration.LastModified

    let private location =
            (function | Some x -> setPLM Response.Headers.location (Location.Location x)
                      | _ -> returnM ())
        =<< configurationKey Configuration.Location

    let private reasonPhrase =
        setPLM Response.reasonPhrase

    let private statusCode =
        setPLM Response.statusCode

    (* Operations *)

    [<RequireQualifiedAccess>]
    module Operation =

        let accepted =
                statusCode 202
             *> reasonPhrase "Accepted"
             *> date

        let badRequest =
                statusCode 400
             *> reasonPhrase "Bad Request"
             *> date

        let conflict =
                statusCode 409
             *> reasonPhrase "Conflict"
             *> date

        let created =
                statusCode 201
             *> reasonPhrase "Created"
             *> date
             *> location

        let forbidden =
                statusCode 403
             *> reasonPhrase "Forbidden"
             *> date

        let gone =
                statusCode 410 
             *> reasonPhrase "Gone"
             *> date

        let methodNotAllowed =
                statusCode 405
             *> reasonPhrase "Method Not Allowed"
             *> allow
             *> date

        let movedPermanently =
                statusCode 301
             *> reasonPhrase "Moved Permanently"
             *> date
             *> location

        let movedTemporarily =
                statusCode 307
             *> reasonPhrase "Moved Temporarily"
             *> date
             *> location

        let multipleRepresentations =
                statusCode 310
             *> reasonPhrase "Multiple Representations"
             *> date

        let noContent =
                statusCode 204
             *> reasonPhrase "No Content"
             *> date

        let notAcceptable =
                statusCode 406
             *> reasonPhrase "Not Acceptable"
             *> date

        let notFound =
                statusCode 404
             *> reasonPhrase "Not Found"
             *> date

        let notImplemented =
                statusCode 501
             *> reasonPhrase "Not Implemented"
             *> date

        let notModified =
                statusCode 304
             *> reasonPhrase "Not Modified"
             *> lastModified
             *> date
             *> eTag
             *> expires

        let ok =
                statusCode 200
             *> reasonPhrase "OK"
             *> lastModified
             *> date
             *> eTag
             *> expires

        let options =
                statusCode 200
             *> reasonPhrase "Options"
             *> lastModified
             *> date
             *> eTag
             *> expires

        let preconditionFailed =
                statusCode 412
             *> reasonPhrase "Precondition Failed"
             *> date

        let requestEntityTooLarge =
                statusCode 413
             *> reasonPhrase "Request Entity Too Large"
             *> date

        let seeOther =
                statusCode 303
             *> reasonPhrase "See Other"
             *> date
             *> location

        let serviceUnavailable =
                statusCode 503
             *> reasonPhrase "Service Unavailable"
             *> date

        let unauthorized =
                statusCode 401
             *> reasonPhrase "Unauthorized"
             *> date

        let unknownMethod =
                statusCode 501
             *> reasonPhrase "Unknown Method"
             *> date

        let unprocessableEntity =
                statusCode 422
             *> reasonPhrase "Unprocessable Entity"
             *> date

        let unsupportedMediaType =
                statusCode 415
             *> reasonPhrase "UnsupportedMediaType"
             *> date

        let uriTooLong =
                statusCode 414
             *> reasonPhrase "URI Too Long"
             *> date