[<AutoOpen>]
module Freya.Core.Prelude

open System.Collections.Generic
open Aether

(* Lenses *)

let dictLens<'k,'v> k : Lens<IDictionary<'k,'v>, 'v> =
    (fun d -> d.[k]),
    (fun v d -> d.[k] <- v; d)

let dictPLens<'k,'v> k : PLens<IDictionary<'k,'v>, 'v> =
    (fun d -> d.TryGetValue k |> function | true, v -> Some v | _ -> None),
    (fun v d -> d.[k] <- v; d)

(* Isomorphisms *)

let boxIso<'a> : Iso<obj, 'a> =
    unbox<'a>, box