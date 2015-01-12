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
module Freya.Core.Utilities

open System
open Aether
open Aether.Operators

(* Lenses *)

let private memoPLens<'a> key =
         CoreState.MetaLens 
    >--> CoreMetaState.MemosLens 
    >-?> mapPLens key
    <?-> boxIso<'a>

(* Memoization *)

/// Memoization for <see cref="CoreState" /> within a <see cref="Core{T}" /> computation.
let memoM<'a> (m: Core<'a>) : Core<'a> =
    let memoPLens = memoPLens (Guid.NewGuid ())

    freyaCore {
        let! memo = getPLM memoPLens

        match memo with
        | Some memo ->
            return memo
        | _ ->
            let! memo = m
            do! setPLM memoPLens memo

            return memo }