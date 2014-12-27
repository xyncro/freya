[<AutoOpen>]
module Freya.Core.Prelude

open System.Collections.Generic
open Aether

(* Lenses *)

/// Defines get and set functions for a lens over an <see cref="IDictionary{T1, T2}" />.
let dictLens<'k,'v> k : Lens<IDictionary<'k,'v>, 'v> =
    (fun d -> d.[k]),
    (fun v d -> d.[k] <- v; d)

/// <summary>
/// Defines get and set functions for a partial lens over an <see cref="IDictionary{k, v}" />.
/// </summary>
/// <remarks>
/// The partial lens uses TryGetValue to retrieve the requested key's value and returns an <see cref="Option{T}" />.
/// The set function will always add or overwrite the value for key k.
/// </remarks>
let dictPLens<'k,'v> k : PLens<IDictionary<'k,'v>, 'v> =
    (fun d -> d.TryGetValue k |> function | true, v -> Some v | _ -> None),
    (fun v d -> d.[k] <- v; d)

(* Isomorphisms *)

/// Provides isomorphisms for boxing and unboxing.
let boxIso<'a> : Iso<obj, 'a> =
    unbox<'a>, box