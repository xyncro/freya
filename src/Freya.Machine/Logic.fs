[<AutoOpen>]
module internal Freya.Machine.Logic

open System
open Aether.Operators
open Freya.Core
open Freya.Core.Operators
open Freya.Types.Cors
open Freya.Types.Http
open Freya.Types.Language

(* Content Negotiation

   Logic for negotiating decisions based on Content Negotiation,
   given suitably configured (optional) definitions of what content
   is supported as part of the Machine Definition. 

   Unlike some other "machine-type" frameworks, all configuration here
   is dynamic not static, and so will be evaluated at runtime, allowing
   for dynamic support based on the specific resource in question. *)

[<RequireQualifiedAccess>]
module Charset =

    let private defaults =
        [ Charset.Iso88591 ]

    let private acceptCharset =
        getPLM Request.Headers.acceptCharset

    let private supported =
            (function | Some x -> x
                      | _ -> defaults)
        <!> config Configuration.CharsetsSupported

    let requested : FreyaMachineDecision =
            Option.isSome 
        <!> acceptCharset

    let negotiated =
            AcceptCharset.negotiate 
        <!> supported
        <*> acceptCharset

    let negotiable : FreyaMachineDecision =
            (function | Negotiated x when List.isEmpty x = false -> true
                      | _ -> false)
        <!> negotiated


[<RequireQualifiedAccess>]
module Encoding =

    let private defaults =
        List.empty<ContentCoding>

    let private acceptEncoding =
        getPLM Request.Headers.acceptEncoding

    let private supported =
            (function | Some x -> x
                      | _ -> defaults)
        <!> config Configuration.EncodingsSupported

    let requested : FreyaMachineDecision =
            Option.isSome 
        <!> acceptEncoding

    let negotiated =
            AcceptEncoding.negotiate 
        <!> supported
        <*> acceptEncoding

    let negotiable : FreyaMachineDecision =
            (function | Negotiated x when List.isEmpty x = false -> true
                      | _ -> false)
        <!> negotiated


[<RequireQualifiedAccess>]
module Language =

    let private defaults =
        List.empty<LanguageTag>

    let private acceptLanguage =
        getPLM Request.Headers.acceptLanguage

    let private supported =
            (function | Some x -> x
                      | _ -> defaults)
        <!> config Configuration.LanguagesSupported

    let requested : FreyaMachineDecision =
            Option.isSome 
        <!> acceptLanguage

    let negotiated =
            AcceptLanguage.negotiate 
        <!> supported
        <*> acceptLanguage

    let negotiable : FreyaMachineDecision =
            (function | Negotiated x when List.isEmpty x = false -> true
                      | _ -> false)
        <!> negotiated


[<RequireQualifiedAccess>]
module MediaType =

    let private defaults =
        List.empty<MediaType>

    let private accept =
        getPLM Request.Headers.accept

    let private supported =
            (function | Some x -> x
                      | _ -> defaults)
        <!> config Configuration.MediaTypesSupported

    let requested : FreyaMachineDecision =
            Option.isSome 
        <!> accept

    let negotiated =
            Accept.negotiate 
        <!> supported
        <*> accept

    let negotiable : FreyaMachineDecision =
            (function | Negotiated x when List.isEmpty x = false -> true
                      | _ -> false)
        <!> negotiated

(* Cache Control

   Logic for negotiating decisions based on Cache Control,
   given suitably configured (optional) definitions of what cache control
   is defined as part of the Machine Definition. 

   Unlike some other "machine-type" frameworks, all configuration here
   is dynamic not static, and so will be evaluated at runtime, allowing
   for dynamic support based on the specific resource in question. *)

[<RequireQualifiedAccess>]
module IfMatch =

    let private ifMatch =
        getPLM Request.Headers.ifMatch

    let requested : FreyaMachineDecision =
            Option.isSome 
        <!> ifMatch

    let any : FreyaMachineDecision =
            (=) (Some (IfMatch IfMatchChoice.Any)) 
        <!> ifMatch


[<RequireQualifiedAccess>]
module IfModifiedSince =

    let private ifModifiedSince =
        getPLM Request.Headers.ifModifiedSince

    let private lastModified =
        config Configuration.LastModified

    let requested : FreyaMachineDecision =
            Option.isSome 
        <!> ifModifiedSince

    let valid : FreyaMachineDecision =
            (function | Some (IfModifiedSince x) -> x < DateTime.UtcNow
                      | _ -> false)
        <!> ifModifiedSince

    let modified =
        freya {
            let! lm = lastModified
            let! ms = ifModifiedSince

            match lm, ms with
            | Some lm, Some (IfModifiedSince ms) -> return! ((<) ms) <!> lm
            | _ -> return false }


[<RequireQualifiedAccess>]
module IfNoneMatch =

    let private ifNoneMatch =
        getPLM Request.Headers.ifNoneMatch

    let requested : FreyaMachineDecision =
            Option.isSome 
        <!> ifNoneMatch

    let any : FreyaMachineDecision =
            (=) (Some (IfNoneMatch IfNoneMatchChoice.Any)) 
        <!> ifNoneMatch


[<RequireQualifiedAccess>]
module IfUnmodifiedSince =

    let private ifUnmodifiedSince =
        getPLM Request.Headers.ifUnmodifiedSince

    let private lastModified =
        config Configuration.LastModified

    let requested : FreyaMachineDecision =
            Option.isSome 
        <!> ifUnmodifiedSince

    let valid : FreyaMachineDecision =
            (function | Some (IfUnmodifiedSince x) -> x < DateTime.UtcNow
                      | _ -> false)
        <!> ifUnmodifiedSince

    let modified =
        freya {
            let! lm = lastModified
            let! us = ifUnmodifiedSince

            match lm, us with
            | Some lm, Some (IfUnmodifiedSince us) -> return! ((>) us) <!> lm
            | _ -> return true }

(* Request *)

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

    let private meth =
        getLM Request.meth

    let private methodsKnown =
            (function | Some x -> Set.ofList x
                      | _ -> defaultKnown) 
        <!> config Configuration.MethodsKnown

    let private methodsSupported =
            (function | Some x -> Set.ofList x
                      | _ -> defaultSupported)
        <!> config Configuration.MethodsSupported

    let known : FreyaMachineDecision =
            Set.contains
        <!> meth 
        <*> methodsKnown

    let supported : FreyaMachineDecision =
            Set.contains
        <!> meth
        <*> methodsSupported

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

(* CORS

   Logic for negotiating decisions based on CORS (Cross-Origin Resource Sharing),
   properly supporting per-resource responses based on genuinely allowable requests,
   available methods outside of the simple set, etc. *)

[<RequireQualifiedAccess>]
module Cors =

    let private accessControlRequestMethod =
        getPLM Request.Headers.accessControlRequestMethod

    let private origin' =
        getPLM Request.Headers.origin

    let private corsOrigins =
        config Configuration.CorsOriginsSupported

    let enabled : FreyaMachineDecision =
            Option.isSome
        <!> corsOrigins

    let origin : FreyaMachineDecision =
            (fun origin origins ->
                match origin, origins with
                | Some _, 
                  Some (AccessControlAllowOriginRange.Any) -> true
                | Some (Origin (OriginListOrNull.Origins (x :: []))), 
                  Some (Origins (OriginListOrNull.Origins xs)) -> List.exists ((=) x) xs
                | _ -> false)
        <!> origin'
        <*> corsOrigins

    let options : FreyaMachineDecision =
        Method.options

    let preflight : FreyaMachineDecision =
            Option.isSome
        <!> accessControlRequestMethod
