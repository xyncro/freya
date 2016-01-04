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
//
//----------------------------------------------------------------------------

/// Core combinator definitions for <see cref="Freya{T}" /> computations.
[<RequireQualifiedAccess>]
module Freya.Core.Freya

open System
open Aether

(* Basic

   Commonly used functions for initialization, conversion, etc. of the
   Freya<'T> function. *)

/// Wraps a value x in a <see cref="Freya{T}" /> computation.
let inline init x : Freya<'T> = 
    fun state -> 
        async.Return (x, state)

/// Applies a function of a value to an <see cref="Async{T}" /> result
/// into a <see cref="Freya{T}" /> computation.
let inline fromAsync f =
    (fun f -> 
        fun state -> 
            async { 
                let! v = f
                return v, state }) << f

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

(* Typeclass *)

type Defaults =
    | Defaults

    static member Freya (x: Freya<_>) =
        x

    static member Freya (_: unit) =
        fun state -> async { return (), state }

let inline defaults (a: ^a, _: ^b) =
        ((^a or ^b) : (static member Freya: ^a -> Freya<_>) a)

let inline infer (x: 'a) =
    defaults (x, Defaults)

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

(* Optic *)

[<RequireQualifiedAccess>]
module Optic =

    /// Gets a value from the Freya State using an Optic
    let inline get o =
        map (Optic.get o) State.get

    /// Sets a value within the Freya State using an Optic
    let inline set o v =
        State.map (Optic.set o v)

    /// Maps a value within the Freya State using an Optic
    let inline map o f =
        State.map (Optic.map o f)

[<RequireQualifiedAccess>]
module Pipeline =

    let next : FreyaPipeline =
        init Next

    let halt : FreyaPipeline =
        init Halt

    type Defaults =
        | Defaults

        static member FreyaPipeline (x: FreyaPipeline) =
            x

        static member FreyaPipeline (x: FreyaPipelineChoice) : FreyaPipeline =
            init x

        static member FreyaPipeline (x: Freya<_>) : FreyaPipeline =
            map2 (fun _ x -> x) x (init Next)

    let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member FreyaPipeline: ^a -> FreyaPipeline) a)

    let inline infer (x: 'a) =
        defaults (x, Defaults)

    /// Composes two pipelines, the second being executed iff the first returns Next
    let inline compose p1 p2 : FreyaPipeline =
        bind (function | Next -> (infer p2) | _ -> halt) (infer p1)

(* Memo

    Functions for memoizing the result of a Freya<'T> function, storing
    the computed result within the FreyaMetaState instance of the
    Freya<'T> state, allowing for computations to be reliably executed
    only once per state (commonly once per request in the usual Freya
    usage model). *)

/// Memoizes a Freya function, executing the function only once per request
let memo<'a> (m: Freya<'a>) : Freya<'a> =
    let memo_ = Memo.id_<'a> (Guid.NewGuid ())

    fun state ->
        async {
            let! memo, state = Optic.get memo_ state

            match memo with
            | Some memo ->
                return memo, state
            | _ ->
                let! memo, state = m state
                let! _, state = Optic.set memo_ (Some memo) state

                return memo, state }

(* Obsolete

   Backwards compatibility shims to make the 2.x-> 3.x transition
   less painful, providing functionally equivalent options where possible.

   To be removed for 4.x releases. *)

(* Pipeline *)

[<Obsolete ("Use Freya.Pipeline.next instead.")>]
let next =
    Pipeline.next

[<Obsolete ("Use Freya.Pipeline.next instead.")>]
let halt =
    Pipeline.halt

[<Obsolete ("Use Freya.Pipeline.compose instead.")>]
let inline pipe p1 p2 =
    Pipeline.compose p1 p2

(* Unqualified State *)

[<Obsolete ("Use Freya.State.get instead.")>]
let getState =
    State.get

[<Obsolete ("Use Freya.State.set instead.")>]
let setState =
    State.set

[<Obsolete ("Use Freya.State.map instead.")>]
let mapState =
    State.map

(* Unqualified Lens *)

[<Obsolete ("Use Freya.Optic.get instead.")>]
let inline getLens l =
    Optic.get l

[<Obsolete ("Use Freya.Optic.get instead.")>]
let inline getLensPartial l =
    Optic.get l

[<Obsolete ("Use Freya.Optic.set instead.")>]
let inline setLens l b =
    Optic.set l b

[<Obsolete ("Use Freya.Optic.set instead.")>]
let inline setLensPartial l b =
    Optic.set l b

[<Obsolete ("Use Freya.Optic.map instead.")>]
let inline mapLens l f =
    Optic.map l f

[<Obsolete ("Use Freya.Optic.map instead.")>]
let inline mapLensPartial l f =
    Optic.map l f

(* Qualified Lens *)

[<RequireQualifiedAccess>]
[<Obsolete ("Use Freya.Optic module functions instead.")>]
module Lens =

    [<Obsolete ("Use Freya.Optic.get instead.")>]
    let inline get l =
        Optic.get l

    [<Obsolete ("Use Freya.Optic.get instead.")>]
    let inline getPartial l =
        Optic.get l

    [<Obsolete ("Use Freya.Optic.set instead.")>]
    let inline set l b =
        Optic.set l b

    [<Obsolete ("Use Freya.Optic.set instead.")>]
    let inline setPartial l b =
        Optic.set l b

    [<Obsolete ("Use Freya.Optic.map instead.")>]
    let inline map l f =
        Optic.map l f

    [<Obsolete ("Use Freya.Optic.map instead.")>]
    let inline mapPartial l f =
        Optic.map l f