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

/// Core combinator definitions for <see cref="Freya{T}" /> computations.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Freya.Core.Freya

open System
open Aether

(* Basic

   Commonly used functions for initialization, conversion, etc. of the
   Freya<'T> function. *)

/// Wraps a value x in a <see cref="Freya{T}" /> computation.
let inline init x : Freya<'T> = 
    fun env -> 
        async.Return (x, env)

/// Applies a function of a value to an <see cref="Async{T}" /> result
/// into a <see cref="Freya{T}" /> computation.
let inline fromAsync f =
    (fun f -> 
        fun env -> 
            async { 
                let! v = f
                return v, env }) << f

/// Binds a <see cref="Freya{T}" /> computation with a function that
/// takes the value from the <see cref="Freya{T}" /> computation and
/// computes a new <see cref="Freya{T}" /> computation of a possibly
/// different type.
let inline bind (f: 'T1 -> Freya<'T2>) (m: Freya<'T1>) : Freya<'T2> =
    fun s -> 
        async { 
            let! r, s' = m s
            return! (f r) s' }

/// Applies a function wrapped in a <see cref="Freya{T}" /> computation
/// onto a <see cref="Freya{T}" /> computation value.
let inline apply f m : Freya<'T> =
    bind (fun f' ->
        bind (fun m' ->
            init (f' m')) m) f

/// Applies a function taking one arguments to one <see cref="Freya{T}" /> computations.
let inline map f m : Freya<'T> =
    bind (fun m' ->
        init (f m')) m

/// Applies a function taking two arguments to two <see cref="Freya{T}" /> computations.
let inline map2 f m1 m2 =
    apply (apply (init f) m1) m2

(* State

   Functions for working with the state within a Freya<'T> function. *)

[<RequireQualifiedAccess>]
module State =

    /// Gets the Freya State within a Freya monad
    let get =
        fun state -> async { return state, state }

    /// Sets the Freya State within a Freya monad
    let set state =
        fun _ -> async { return (), state }

    /// Modifies the Freya State within a Freya monad
    let map f =
        fun state -> async { return (), f state }

(* Deprecated state functionality, to be removed in a future
   release. *)

[<Obsolete ("Use Freya.State.get instead.")>]
let getState =
    State.get

[<Obsolete ("Use Freya.State.set instead.")>]
let setState =
    State.set

[<Obsolete ("Use Freya.State.map instead.")>]
let mapState =
    State.map

(* Lens

   Functions for working with the state within a Freya<'T> function, using
   Aether based lenses. *)

[<RequireQualifiedAccess>]
module Lens =

    /// Gets part of the Core State within a Core monad using an Aether lens
    let get l = 
        map (Lens.get l) State.get

    /// Gets part of the Core State within a Core monad using a partial Aether lens
    let getPartial l = 
        map (Lens.getPartial l) State.get

    /// Sets part of the Core State within a Core monad using an Aether lens
    let set l v =
        State.map (Lens.set l v)

    /// Sets part of the Core State within a Core monad using a partial Aether lens
    let setPartial l v = 
        State.map (Lens.setPartial l v)

    /// Modifies part of the Core State within a Core monad using an Aether lens
    let map l f = 
        State.map (Lens.map l f)

    /// Modifies part of the Core State within a Core monad using a partial Aether lens
    let mapPartial l f = 
        State.map (Lens.mapPartial l f)

(* Deprecated lens functionality, to be removed in a future
   release. *)

[<Obsolete ("Use Freya.Lens.get instead.")>]
let getLens =
    Lens.get

[<Obsolete ("Use Freya.Lens.getPartial instead.")>]
let getLensPartial =
    Lens.getPartial

[<Obsolete ("Use Freya.Lens.set instead.")>]
let setLens =
    Lens.set

[<Obsolete ("Use Freya.Lens.setPartial instead.")>]
let setLensPartial =
    Lens.setPartial

[<Obsolete ("Use Freya.Lens.map instead.")>]
let mapLens =
    Lens.map

[<Obsolete ("Use Freya.Lens.mapPartial instead.")>]
let mapLensPartial =
    Lens.mapPartial

(* Memo

   Functions for memoizing the result of a Freya<'T> function, storing
   the computed result within the FreyaMetaState instance of the
   Freya<'T> state, allowing for computations to be reliably executed
   only once per state (commonly once per request in the usual Freya
   usage model). *)

let memo<'a> (m: Freya<'a>) : Freya<'a> =
    let memo_ = Memo.Id_<'a> (Guid.NewGuid ())

    fun state ->
        async {
            let! memo, state = Lens.getPartial memo_ state

            match memo with
            | Some memo ->
                return memo, state
            | _ ->
                let! memo, state = m state
                let! _, state = Lens.setPartial memo_ memo state

                return memo, state }

(* Pipeline *)

let next : FreyaPipeline =
    init Next

let halt : FreyaPipeline =
    init Halt

let pipe (p1: FreyaPipeline) (p2: FreyaPipeline) : FreyaPipeline =
    bind (function | Next -> p2 | _ -> halt) p1