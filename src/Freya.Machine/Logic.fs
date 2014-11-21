[<AutoOpen>]
module internal Freya.Machine.Logic

open System
open System.Globalization
open Aether.Operators
open Freya.Core
open Freya.Core.Operators
open Freya.Typed

(* Operators *)

let inline private (!?) x =
    Option.isSome <!> x

let inline private (!.) x =
    List.isEmpty >> not <!> x

let inline private (=?) x y =
    (=) y <!> x

let inline private (?>) (x, y) f =
    (fun x y -> f (x, y)) <!> x <*> y

(* Configuration *)

let private config key =
    getPLM (definitionPLens >??> configurationPLens key)

(* Content Negotiation *)

let private negotiate f defaults =
    function | Some r, Some a -> f r a
             | Some r, _ -> f r defaults
             | _, Some a -> a
             | _ -> defaults


[<RequireQualifiedAccess>]
module Charset =

    let private defaults =
        [ Charsets.Iso88591 ]

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
        List.empty<Encoding>

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
        List.empty<CultureInfo>
        
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

(* Control *)

[<RequireQualifiedAccess>]
module IfMatch =

    let private ifMatch =
        getPLM Request.Headers.ifMatch

    let requested : FreyaMachineDecision =
        !? ifMatch

    let any : FreyaMachineDecision =
        ifMatch =? Some IfMatch.Any


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
        ifNoneMatch =? Some IfNoneMatch.Any


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
        config Configuration.MethodsKnown

    let private negotiateKnown =
        function | x, Some y -> Set.contains x y
                 | x, _ -> Set.contains x defaultKnown

    let private methodsSupported =
        config Configuration.MethodsSupported

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
