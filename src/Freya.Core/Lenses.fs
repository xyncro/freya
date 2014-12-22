[<AutoOpen>]
module Freya.Core.Lenses

open Aether
open Aether.Operators

(* Environment Lenses *)

let environmentKeyLens<'a> key =
    FreyaState.EnvironmentLens >--> mutDictLens<string, obj> key <--> boxIso<'a>

let environmentKeyPLens<'a> key =
    FreyaState.EnvironmentLens >-?> mutDictPLens<string, obj> key <?-> boxIso<'a>