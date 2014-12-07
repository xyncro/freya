[<AutoOpen>]
module internal Freya.Machine.Logic

open System
open Aether.Operators
open Freya.Core
open Freya.Core.Operators
open Freya.Types.Http

(* Functions *)

let private applyTupleM f (x, y) =
    (fun x y -> f (x, y)) <!> x <*> y

let private eqM x y =
    (=) y <!> x

let private inM x y =
    flip Set.contains y <!> x

let private someM x =
    Option.isSome <!> x

let private someNotEmptyM x =
    (function | Some x -> not (List.isEmpty x) | _ -> false) <!> x

(* Configuration

   Typed access to dynamic configuration values at runtime. These are
   evaluated on machine execution, and so may be varied based on the
   specific resource in question (they are a general core Freya<'T> 
   expression). *)

let private configM key =
    freya {
        let! value = getPLM (definitionPLens >??> configurationPLens key)

        match value with
        | Some value -> return! Some <!> value
        | _ -> return None }

let private configOrElseM key def =
    (function | Some x -> Some x | _ -> Some def ) <!> configM key

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

    let private request =
        getPLM Request.Headers.acceptCharset

    let private supported =
        configOrElseM Configuration.CharsetsSupported defaults

    let requested : FreyaMachineDecision =
        someM request

    let negotiated =
        applyTupleM AcceptCharset.negotiate (supported, request)

    let negotiable : FreyaMachineDecision =
        someNotEmptyM negotiated


[<RequireQualifiedAccess>]
module Encoding =

    let private request =
        getPLM Request.Headers.acceptEncoding

    let private supported =
        configM Configuration.EncodingsSupported

    let requested : FreyaMachineDecision =
        someM request

    let negotiated =
        applyTupleM AcceptEncoding.negotiate (supported, request)

    let negotiable : FreyaMachineDecision =
        someNotEmptyM negotiated


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

    let private request =
        getPLM Request.Headers.acceptLanguage

    let private supported =
        configM Configuration.LanguagesSupported

    let requested : FreyaMachineDecision =
        someM request

    let negotiated =
        applyTupleM AcceptLanguage.negotiate (supported, request)

    let negotiable : FreyaMachineDecision =
        someNotEmptyM negotiated


[<RequireQualifiedAccess>]
module MediaType =

    let private defaults =
        List.empty<MediaType>

    let private request =
        getPLM Request.Headers.accept

    let private supported =
        configM Configuration.MediaTypesSupported

    let requested : FreyaMachineDecision =
        someM request

    let negotiated =
        applyTupleM Accept.negotiate (supported, request)

    let negotiable : FreyaMachineDecision =
        someNotEmptyM negotiated

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
        someM ifMatch

    let any : FreyaMachineDecision =
        eqM ifMatch (Some (IfMatch IfMatchChoice.Any))


[<RequireQualifiedAccess>]
module IfModifiedSince =

    let private ifModifiedSince =
        getPLM Request.Headers.ifModifiedSince

    let private lastModified =
        configM Configuration.LastModified

    let requested : FreyaMachineDecision =
        someM ifModifiedSince

    let valid : FreyaMachineDecision =
            Option.map (fun (IfModifiedSince x) -> x < DateTime.UtcNow) >> Option.getOrElse false
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
        someM ifNoneMatch

    let any : FreyaMachineDecision =
        eqM ifNoneMatch (Some (IfNoneMatch IfNoneMatchChoice.Any))


[<RequireQualifiedAccess>]
module IfUnmodifiedSince =

    let private ifUnmodifiedSince =
        getPLM Request.Headers.ifUnmodifiedSince

    let private lastModified =
        configM Configuration.LastModified

    let requested : FreyaMachineDecision =
        someM ifUnmodifiedSince

    let valid : FreyaMachineDecision =
            Option.map (fun (IfUnmodifiedSince x ) -> x < DateTime.UtcNow) >> Option.getOrElse false
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
        Option.map Set.ofList <!> configM Configuration.MethodsKnown

    let private negotiateKnown =
        function | x, Some y -> Set.contains x y
                 | x, _ -> Set.contains x defaultKnown

    let private methodsSupported =
        Option.map Set.ofList <!> configM Configuration.MethodsSupported

    let private negotiateSupported =
        function | x, Some y -> Set.contains x y
                 | x, _ -> Set.contains x defaultSupported

    let known : FreyaMachineDecision =
        applyTupleM negotiateKnown (meth, methodsKnown)

    let supported : FreyaMachineDecision =
        applyTupleM negotiateSupported (meth, methodsSupported)

    let delete : FreyaMachineDecision =
        eqM meth DELETE 

    let getOrHead : FreyaMachineDecision =
        inM meth (set [ GET; HEAD ])

    let options : FreyaMachineDecision =
        eqM meth OPTIONS

    let patch : FreyaMachineDecision =
        eqM meth PATCH

    let post : FreyaMachineDecision =
        eqM meth POST

    let put : FreyaMachineDecision =
        eqM meth PUT