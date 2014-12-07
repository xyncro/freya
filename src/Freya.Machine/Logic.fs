[<AutoOpen>]
module internal Freya.Machine.Logic

open System
open Aether.Operators
open Freya.Core
open Freya.Core.Operators
open Freya.Types.Http
open Freya.Types.Language

(* Configuration

   Typed access to dynamic configuration values at runtime. These are
   evaluated on machine execution, and so may be varied based on the
   specific resource in question (they are a general core Freya<'T> 
   expression). *)

let private config key =
    freya {
        let! value = getPLM (definitionPLens >??> configurationPLens key)

        match value with
        | Some value -> return! Some <!> value
        | _ -> return None }

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
            (function | Some x when List.isEmpty x = false -> true
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
            (function | Some x when List.isEmpty x = false -> true
                      | _ -> false)
        <!> negotiated


[<RequireQualifiedAccess>]
module Language =

    (* Language Logic
    
       In the case of language negotiation, Freya.Machine will negotiate in 
       the following ways:

       If languages supported are specified as part of the resource,
       and acceptable languages are included as part of the request, language
       negotiation will occur. The result of this negotiation will be returned
       as Some <results> where results is the list of supported language tags
       which have been negotiated as acceptable, ordered by the priorities (if any)
       specified as part of the acceptable languages requested.

       If languages supported, or languages acceptable are not specified,
       as part of the resource or request respectively, no negotiation will
       be attempted, and None will be returned as the result of negotiation,
       signifying that no languages have been negotiated.

       Note that in the case of negotiation being defined as "non-strict",
       the default for language support, this may result in an empty negotiation
       result when it comes to deciding on the representation. This is valid, and
       signifies the valid HTTP case where the wishes of the client cannot be fulfilled,
       but the request will be honored with the best intention of the server. *)

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
            (function | Some x when List.isEmpty x = false -> true
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
            (function | Some x when List.isEmpty x = false -> true
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
            Option.map Set.ofList 
        <!> config Configuration.MethodsKnown

    let private methodsSupported =
            Option.map Set.ofList 
        <!> config Configuration.MethodsSupported

    let known : FreyaMachineDecision =
            (function | Some x -> flip Set.contains x
                      | _ -> flip Set.contains defaultKnown)
        <!> methodsKnown 
        <*> meth

    let supported : FreyaMachineDecision =
            (function | Some x -> flip Set.contains x
                      | _ -> flip Set.contains defaultSupported)
        <!> methodsSupported
        <*> meth

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
