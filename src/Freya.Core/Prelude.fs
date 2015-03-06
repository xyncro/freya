//----------------------------------------------------------------------------
//
// Copyright (c) 2014
//
//    Ryan Riley (@panesofglass) and Andrew Cherry (@kolektiv)
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//----------------------------------------------------------------------------

[<AutoOpen>]
module Freya.Core.Prelude

open System.Collections.Generic
open Aether

(* Equality/Comparison

   Functions for simplifying the customization of equality
   and comparison on types where this is required. *)

let equalsOn f x (y: obj) =
    match y with
    | :? 'T as y -> (f x) = (f y)
    | _ -> false
 
let hashOn f x = 
    hash (f x)
 
let compareOn f x (y: obj) =
    match y with
    | :? 'T as y -> compare (f x) (f y)
    | _ -> invalidArg "y" "cannot compare values of different types"

(* Lenses *)

/// Defines get and set functions for a <see cref="Lens{T1, T2}" /> over an <see cref="IDictionary{T1, T2}" />.
let mutDictLens<'k,'v> k : Lens<IDictionary<'k,'v>, 'v> =
    (fun d -> d.[k]),
    (fun v d -> d.[k] <- v; d)

/// <summary>
/// Defines get and set functions for a partial lens, <see cref="PLens{T1, T2}" />, over an <see cref="IDictionary{T1, T2}" />.
/// </summary>
/// <remarks>
/// The partial lens uses TryGetValue to retrieve the requested key's value and returns an <see cref="Option{T}" />.
/// The set function will always add or overwrite the value for key k.
/// </remarks>
let mutDictPLens<'k,'v> k : PLens<IDictionary<'k,'v>, 'v> =
    (fun d -> d.TryGetValue k |> function | true, v -> Some v | _ -> None),
    (fun v d -> d.[k] <- v; d)

(* Isomorphisms *)

/// Provides isomorphisms for boxing and unboxing.
let boxIso<'a> : Iso<obj, 'a> =
    unbox<'a>, box

(* Functions *)

let inline flip f a b = 
    f b a

(* Option Extensions *)

[<RequireQualifiedAccess>]
module Option =
    
    let orElse def =
        function | Some x -> x
                 | _ -> def