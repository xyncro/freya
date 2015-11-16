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

(* Isomorphisms *)

/// Provides isomorphisms for boxing and unboxing.
let box_<'a> : Isomorphism<obj, 'a> =
    unbox<'a>, box

(* Dict Extensions *)

[<RequireQualifiedAccess>]
module Dict =

    let value_<'k,'v> k : Lens<IDictionary<'k,'v>, 'v option> =
        (fun d ->
            match d.TryGetValue k with
            | true, v -> Some v
            | _ -> None),
        (fun v d ->
            match v with
            | Some v -> d.[k] <- v; d
            | _ -> d)

(* Functions *)

let inline flip f a b =
    f b a

(* Option Extensions *)

[<RequireQualifiedAccess>]
module Option =

    let mapIsomorphism (i: Isomorphism<'a,'b>) : Isomorphism<'a option, 'b option> =
        Option.map (fst i), Option.map (snd i)

    let orElse def =
        function | Some x -> x
                 | _ -> def