[<AutoOpen>]
module internal Dyfrig.Machine.Logic

open System
open System.Globalization
open Aether.Operators
open Dyfrig.Core
open Dyfrig.Core.Operators
open Dyfrig.Http

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

(* Negotiation *)

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

    let requested : MachineDecision =
        !? request

    let negotiated =
        (request, supported) ?> negotiate AcceptCharset.negotiate defaults

    let negotiable : MachineDecision =
        !. negotiated


[<RequireQualifiedAccess>]
module Encoding =
        
    let private defaults =
        List.empty<Encoding>

    let private request =
        getPLM Request.Headers.acceptEncoding

    let private supported =
        config Configuration.EncodingsSupported

    let requested : MachineDecision =
        !? request

    let negotiated =
        (request, supported) ?> negotiate AcceptEncoding.negotiate defaults

    let negotiable : MachineDecision =
        !. negotiated 


[<RequireQualifiedAccess>]
module Language =

    let private defaults =
        List.empty<CultureInfo>
        
    let private request =
        getPLM Request.Headers.acceptLanguage

    let private supported =
        config Configuration.LanguagesSupported

    let requested : MachineDecision =
        !? request

    let negotiated =
        (request, supported) ?> negotiate AcceptLanguage.negotiate defaults

    let negotiable : MachineDecision =
        !. negotiated


[<RequireQualifiedAccess>]
module MediaType =

    let private defaults =
        List.empty<MediaType>

    let private request =
        getPLM Request.Headers.accept

    let private supported =
        config Configuration.MediaTypesSupported

    let requested : MachineDecision =
        !? request

    let negotiated =
        (request, supported) ?> negotiate Accept.negotiate defaults

    let negotiable : MachineDecision =
        !. negotiated

(* Control *)

[<RequireQualifiedAccess>]
module IfMatch =

    let private ifMatch =
        getPLM Request.Headers.ifMatch

    let requested : MachineDecision =
        !? ifMatch

    let any : MachineDecision =
        ifMatch =? Some IfMatch.Any


[<RequireQualifiedAccess>]
module IfModifiedSince =

    let private ifModifiedSince =
        getPLM Request.Headers.ifModifiedSince

    let private lastModified =
        config Configuration.LastModified

    let requested : MachineDecision =
        !? ifModifiedSince

    let valid : MachineDecision =
            Option.map (fun (IfModifiedSince x) -> x < DateTime.UtcNow) >> Option.getOrElse false
        <!> ifModifiedSince

    let modified =
        owin {
            let! lm = lastModified
            let! ms = ifModifiedSince

            match lm, ms with
            | Some lm, Some (IfModifiedSince ms) -> return! ((<) ms) <!> lm
            | _ -> return false }


[<RequireQualifiedAccess>]
module IfNoneMatch =

    let private ifNoneMatch =
        getPLM Request.Headers.ifNoneMatch

    let requested : MachineDecision =
        !? ifNoneMatch

    let any : MachineDecision =
        ifNoneMatch =? Some IfNoneMatch.Any


[<RequireQualifiedAccess>]
module IfUnmodifiedSince =

    let private ifUnmodifiedSince =
        getPLM Request.Headers.ifUnmodifiedSince

    let private lastModified =
        config Configuration.LastModified

    let requested : MachineDecision =
        !? ifUnmodifiedSince

    let valid : MachineDecision =
            Option.map (fun (IfUnmodifiedSince x ) -> x < DateTime.UtcNow) >> Option.getOrElse false
        <!> ifUnmodifiedSince

    let modified =
        owin {
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

    let known : MachineDecision =
        (meth, methodsKnown) ?> negotiateKnown

    let supported : MachineDecision =
        (meth, methodsSupported) ?> negotiateSupported

    let delete : MachineDecision =
        meth =? DELETE 

    let getOrHead : MachineDecision =
            (fun x -> x = GET || x = HEAD) 
        <!> meth

    let options : MachineDecision =
        meth =? OPTIONS

    let patch : MachineDecision =
        meth =? PATCH

    let post : MachineDecision =
        meth =? POST

    let put : MachineDecision =
        meth =? PUT