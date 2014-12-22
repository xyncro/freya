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

        [<RequireQualifiedAccess>]
        module Decision =

            let requested : FreyaMachineDecision =
                    Option.isSome 
                <!> getPLM Request.Headers.ifMatch

            let any : FreyaMachineDecision =
                    (=) (Some (IfMatch IfMatchChoice.Any)) 
                <!> getPLM Request.Headers.ifMatch

    (* If-Modified-Since *)

    [<RequireQualifiedAccess>]
    module IfModifiedSince =

        [<RequireQualifiedAccess>]
        module Decision =

            let requested : FreyaMachineDecision =
                    Option.isSome 
                <!> getPLM Request.Headers.ifModifiedSince

            let valid : FreyaMachineDecision =
                    (function | Some (IfModifiedSince x) -> x < DateTime.UtcNow
                              | _ -> false)
                <!> getPLM Request.Headers.ifModifiedSince

            let modified : FreyaMachineDecision =
                    (fun x y -> (function | Some lm, Some (IfModifiedSince ms) -> lm > ms
                                          | _ -> false) (x, y))
                <!> configurationKey Configuration.LastModified
                <*> getPLM Request.Headers.ifModifiedSince

    (* If-None-Match *)

    [<RequireQualifiedAccess>]
    module IfNoneMatch =

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

        let private ifUnmodifiedSince =
            getPLM Request.Headers.ifUnmodifiedSince

        let private lastModified =
            configurationKey Configuration.LastModified

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

        let private defaults =
            [ Charset.Iso88591 ]

        let private supported =
                (function | Some x -> x
                          | _ -> defaults)
            <!> configurationKey Configuration.CharsetsSupported

        (* Negotiation *)

        [<RequireQualifiedAccess>]
        module Negotiation =

            let negotiated =
                    (fun x y -> (function | Some (AcceptCharset x), y -> Negotiated (run x y)
                                          | _ -> Free) (x, y))
                <!> getPLM Request.Headers.acceptCharset
                <*> supported

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

        let private negotiate supported=
            function | Some (AcceptEncoding requested) -> Negotiated (run requested supported)
                     | _ -> Free

        let private defaults =
            List.empty<ContentCoding>

        let private acceptEncoding =
            getPLM Request.Headers.acceptEncoding

        let private supported =
                (function | Some x -> x
                            | _ -> defaults)
            <!> configurationKey Configuration.EncodingsSupported

        [<RequireQualifiedAccess>]
        module Negotiation =

            let negotiated =
                    negotiate 
                <!> supported
                <*> acceptEncoding

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

        let private toList tag =
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
            Seq.zip (toList tag) >> Seq.forall ((<||) (==))

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

        let private negotiate supported =
            function | Some (AcceptLanguage requested) -> Negotiated (run supported requested)
                     | _ -> Free

        let private defaults =
            List.empty<LanguageTag>

        let private acceptLanguage =
            getPLM Request.Headers.acceptLanguage

        let private supported =
                (function | Some x -> x
                          | _ -> defaults)
            <!> configurationKey Configuration.LanguagesSupported

        [<RequireQualifiedAccess>]
        module Negotiation =

            let negotiated =
                    negotiate 
                <!> supported
                <*> acceptLanguage

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

        let private negotiate supported =
            function | Some (Accept requested) -> Negotiated (run requested supported)
                     | _ -> Free

        let private defaults =
            List.empty<MediaType>

        let private accept =
            getPLM Request.Headers.accept

        let private supported =
                (function | Some x -> x
                          | _ -> defaults)
            <!> configurationKey Configuration.MediaTypesSupported

        [<RequireQualifiedAccess>]
        module Negotiation =

            let negotiated =
                    negotiate 
                <!> supported
                <*> accept

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

    (* Decisions *)

    [<RequireQualifiedAccess>]
    module Decision =

        let enabled : FreyaMachineDecision =
                Option.isSome
            <!> configurationKey Configuration.CorsOriginsSupported

        let origin : FreyaMachineDecision =
                (fun origin origins ->
                    match origin, origins with
                    | Some _, 
                      Some (AccessControlAllowOriginRange.Any) -> true
                    | Some (Origin (OriginListOrNull.Origins (x :: []))), 
                      Some (Origins (OriginListOrNull.Origins xs)) -> List.exists ((=) x) xs
                    | _ -> false)
            <!> getPLM Request.Headers.origin   
            <*> configurationKey Configuration.CorsOriginsSupported

        let options : FreyaMachineDecision =
                (=) OPTIONS 
            <!> getLM Request.meth

        let preflight : FreyaMachineDecision =
                Option.isSome
            <!> getPLM Request.Headers.accessControlRequestMethod

    (* Operations *)

    [<RequireQualifiedAccess>]
    module Operation =

        // TODO: Refactor Operations

        let private headersExposed =
                (function | Some x -> x
                          | _ -> [])
            <!> configurationKey Configuration.CorsHeadersExposed

        let private headersSupported =
                (function | Some x -> x
                          | _ -> [])
            <!> configurationKey Configuration.CorsHeadersSupported

        let private methodsSupported =
                (function | Some x -> x
                          | _ -> [])
            <!> configurationKey Configuration.CorsMethodsSupported

        let private accessControlRequestHeaders =
                (function | Some (AccessControlRequestHeaders x) -> x
                          | _ -> [])
            <!> getPLM Req.accessControlRequestHeaders

        let private accessControlRequestMethod' =
                (Option.map (fun (AccessControlRequestMethod x) -> x) >> Option.get)
            <!> getPLM Req.accessControlRequestMethod

        let private origin' =
                (Option.map (fun (Origin x) -> x) >> Option.get) 
            <!> getPLM Req.origin

        let private headersAllowed =
                (fun headers supported ->
                    match List.forall (fun x -> List.exists ((=) x) supported) headers with
                    | true -> headers
                    | _ -> [])
            <!> accessControlRequestHeaders
            <*> headersSupported

        let private methodsAllowed =
                (fun meth supported ->
                    match List.exists ((=) meth) supported with
                    | true -> [ meth ]
                    | _ -> [])
            <!> accessControlRequestMethod'
            <*> methodsSupported

        let private setAccessControlAllowMethods =
            setPLM Res.accessControlAllowMethods =<< (AccessControlAllowMethods <!> methodsAllowed)

        let private setAccessControlExposeHeaders =
            setPLM Res.accessControlExposeHeaders =<< (AccessControlExposeHeaders <!> headersExposed)

        let private setAccessControlAllowHeaders =
            setPLM Res.accessControlAllowHeaders =<< (AccessControlAllowHeaders <!> headersAllowed)

        let private setOrigin =
            setPLM Res.accessControlAllowOrigin =<< ((Origins >> AccessControlAllowOrigin) <!> origin')

        let actual =
            setAccessControlExposeHeaders

        let origin =
            setOrigin

        let preflight =
            setAccessControlAllowMethods *> setAccessControlAllowHeaders

(* Method *)

[<RequireQualifiedAccess>]
module Method =

    let private defaultKnown =
        Set.ofList [ 
            DELETE
            HEAD
            GET
            OPTIONS
            PATCH
            POST
            PUT
            TRACE ]

    let private defaultSupported =
        Set.ofList [ 
            GET
            HEAD ]

    let private methodsKnown =
            (function | Some x -> Set.ofList x
                      | _ -> defaultKnown) 
        <!> configurationKey Configuration.MethodsKnown

    let private methodsSupported =
            (function | Some x -> Set.ofList x
                      | _ -> defaultSupported)
        <!> configurationKey Configuration.MethodsSupported

    (* Decisions *)

    [<RequireQualifiedAccess>]
    module Decision =

        let known : FreyaMachineDecision =
                Set.contains
            <!> getLM Request.meth 
            <*> methodsKnown

        let supported : FreyaMachineDecision =
                Set.contains
            <!> getLM Request.meth
            <*> methodsSupported

        let delete : FreyaMachineDecision =
                (=) DELETE 
            <!> getLM Request.meth

        let getOrHead : FreyaMachineDecision =
                flip Set.contains (set [ GET; HEAD ])
            <!> getLM Request.meth

        let options : FreyaMachineDecision =
                (=) OPTIONS 
            <!> getLM Request.meth

        let patch : FreyaMachineDecision =
                (=) PATCH 
            <!> getLM Request.meth

        let post : FreyaMachineDecision =
                (=) POST 
            <!> getLM Request.meth

        let put : FreyaMachineDecision =
                (=) PUT 
            <!> getLM Request.meth