[<AutoOpen>]
module Freya.Core.Lenses

open Aether
open Aether.Operators

(* Environemnt Lenses *)

let environmentKey<'a> key : Lens<FreyaEnvironment, 'a> =
    dictLens<string, obj> key <--> boxIso<'a>

let environmentKeyP<'a> key : PLens<FreyaEnvironment, 'a> =
    dictPLens<string, obj> key <?-> boxIso<'a>