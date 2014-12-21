[<AutoOpen>]
module Freya.Core.Utilities

open System
open Aether
open Aether.Operators

(* Lenses *)

let private memoPLens id =
    metaLens >--> memosLens >-?> mapPLens id

(* Memoization *)

let memoM<'a> (m: Freya<'a>) : Freya<'a> =
    let id = Guid.NewGuid ()
    let memoPLens = memoPLens id <?-> boxIso<'a>
     
    freya {
        let! memo = getPLM memoPLens

        match memo with
        | Some memo ->
            return memo
        | _ ->
            let! memo = m
            do! setPLM memoPLens memo

            return memo }