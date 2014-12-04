[<AutoOpen>]
module internal Freya.Machine.Logic

open System
open Aether.Operators
open Freya.Core
open Freya.Core.Operators
open Freya.Types
open Freya.Types.Http
open Freya.Types.Language

(* Operators *)

let inline private (!?) x =
    Option.isSome <!> x

let inline private (!.) x =
    List.isEmpty >> not <!> x

let inline private (=?) x y =
    (=) y <!> x

let inline private (?>) (x, y) f =
    (fun x y -> f (x, y)) <!> x <*> y

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

let private negotiate f defaults =
    function | Some r, Some a -> f r a
             | Some r, _ -> f r defaults
             | _, Some a -> a
             | _ -> defaults


[<RequireQualifiedAccess>]
module Charset =

    let private defaults =
        [ Charset.Iso88591 ]

    let private request =
        getPLM Request.Headers.acceptCharset

    let private supported =
        config Configuration.CharsetsSupported

    let requested : FreyaMachineDecision =
        !? request

    let negotiated =
        (request, supported) ?> negotiate AcceptCharset.negotiate defaults

    let negotiable : FreyaMachineDecision =
        !. negotiated


[<RequireQualifiedAccess>]
module Encoding =
        
    let private defaults =
        List.empty<ContentCoding>

    let private request =
        getPLM Request.Headers.acceptEncoding

    let private supported =
        config Configuration.EncodingsSupported

    let requested : FreyaMachineDecision =
        !? request

    let negotiated =
        (request, supported) ?> negotiate AcceptEncoding.negotiate defaults

    let negotiable : FreyaMachineDecision =
        !. negotiated


[<RequireQualifiedAccess>]
module Language =

    let private defaults =
        List.empty<LanguageTag>
        
    let private request =
        getPLM Request.Headers.acceptLanguage

    let private supported =
        config Configuration.LanguagesSupported

    let requested : FreyaMachineDecision =
        !? request

    let negotiated =
        (request, supported) ?> negotiate AcceptLanguage.negotiate defaults

    let negotiable : FreyaMachineDecision =
        !. negotiated


[<RequireQualifiedAccess>]
module MediaType =

    let private defaults =
        List.empty<MediaType>

    let private request =
        getPLM Request.Headers.accept

    let private supported =
        config Configuration.MediaTypesSupported

    let requested : FreyaMachineDecision =
        !? request

    let negotiated =
        (request, supported) ?> negotiate Accept.negotiate defaults

    let negotiable : FreyaMachineDecision =
        !. negotiated

(* Content Negotiation

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
        !? ifMatch

    let any : FreyaMachineDecision =
        ifMatch =? Some (IfMatch IfMatchChoice.Any)


[<RequireQualifiedAccess>]
module IfModifiedSince =

    let private ifModifiedSince =
        getPLM Request.Headers.ifModifiedSince

    let private lastModified =
        config Configuration.LastModified

    let requested : FreyaMachineDecision =
        !? ifModifiedSince

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
        !? ifNoneMatch

    let any : FreyaMachineDecision =
        ifNoneMatch =? Some (IfNoneMatch IfNoneMatchChoice.Any)


[<RequireQualifiedAccess>]
module IfUnmodifiedSince =

    let private ifUnmodifiedSince =
        getPLM Request.Headers.ifUnmodifiedSince

    let private lastModified =
        config Configuration.LastModified

    let requested : FreyaMachineDecision =
        !? ifUnmodifiedSince

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
            Option.map Set.ofList 
        <!> config Configuration.MethodsKnown

    let private negotiateKnown =
        function | x, Some y -> Set.contains x y
                 | x, _ -> Set.contains x defaultKnown

    let private methodsSupported =
            Option.map Set.ofList 
        <!> config Configuration.MethodsSupported

    let private negotiateSupported =
        function | x, Some y -> Set.contains x y
                 | x, _ -> Set.contains x defaultSupported

    let known : FreyaMachineDecision =
        (meth, methodsKnown) ?> negotiateKnown

    let supported : FreyaMachineDecision =
        (meth, methodsSupported) ?> negotiateSupported

    let delete : FreyaMachineDecision =
        meth =? DELETE 

    let getOrHead : FreyaMachineDecision =
            (fun x -> x = GET || x = HEAD) 
        <!> meth

    let options : FreyaMachineDecision =
        meth =? OPTIONS

    let patch : FreyaMachineDecision =
        meth =? PATCH

    let post : FreyaMachineDecision =
        meth =? POST

    let put : FreyaMachineDecision =
        meth =? PUT