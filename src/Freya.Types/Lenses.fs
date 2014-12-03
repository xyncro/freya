[<AutoOpen>]
module Freya.Types.Lenses

open System.Collections.Generic
open Aether
open Aether.Operators

(* Common Lenses *)

let inline dictLens<'k,'v> k : Lens<IDictionary<'k,'v>, 'v> =
    (fun d -> d.[k]),
    (fun v d -> d.[k] <- v; d)

let inline dictPLens<'k,'v> k : PLens<IDictionary<'k,'v>, 'v> =
    (fun d -> d.TryGetValue k |> function | true, v -> Some v | _ -> None),
    (fun v d -> d.[k] <- v; d)

let itemLens<'a> key =
    dictLens<string, obj> key <--> boxIso<'a>

let itemPLens<'a> key =
    dictPLens<string, obj> key <?-> boxIso<'a>