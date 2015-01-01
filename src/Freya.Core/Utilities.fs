[<AutoOpen>]
module Freya.Core.Utilities

open System
open Aether
open Aether.Operators

(* Lenses *)

let private memoPLens<'a> key =
         FreyaState.MetaLens 
    >--> FreyaMetaState.MemosLens 
    >-?> mapPLens key
    <?-> boxIso<'a>

(* Memoization *)

let memoM<'a> (m: Freya<'a>) : Freya<'a> =
    let memoPLens = memoPLens (Guid.NewGuid ())
     
    freya {
        let! memo = getPLM memoPLens

        match memo with
        | Some memo ->
            return memo
        | _ ->
            let! memo = m
            do! setPLM memoPLens memo

            return memo }