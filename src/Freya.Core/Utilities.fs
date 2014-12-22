[<AutoOpen>]
module Freya.Core.Utilities

open System
open Aether
open Aether.Operators

(* Lenses *)

let private memoPLens key =
    FreyaState.MetaLens >--> FreyaMetaState.MemosLens >-?> mapPLens key

(* Memoization *)

let memoM<'a> (m: Freya<'a>) : Freya<'a> =
    let key = Guid.NewGuid ()
    let memoPLens = memoPLens key <?-> boxIso<'a>
     
    freya {
        let! memo = getPLM memoPLens

        match memo with
        | Some memo ->
            return memo
        | _ ->
            let! memo = m
            do! setPLM memoPLens memo

            return memo }