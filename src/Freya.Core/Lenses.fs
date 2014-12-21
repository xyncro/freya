[<AutoOpen>]
module Freya.Core.Lenses

open Aether
open Aether.Operators

(* Environment Lenses *)

let environmentKeyLens<'a> key : Lens<FreyaState, 'a> =
    environmentLens >--> mutDictLens<string, obj> key <--> boxIso<'a>

let environmentKeyPLens<'a> key : PLens<FreyaState, 'a> =
    environmentLens >-?> mutDictPLens<string, obj> key <?-> boxIso<'a>