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
module Freya.Core.Functions

open Aether
open Freya.Core.Operators

(* State Functions *)

/// Gets the Freya State within a Freya monad
let getM =
    fun e -> async { return e, e }

/// Sets the Freya State within a Freya monad
let setM e =
    fun _ -> async { return (), e }

/// Modifies the Freya State within a Freya monad
let modM f =
    fun e -> async { return (), f e }

(* State Lens Functions *)

/// Gets part of the Freya State within a Freya monad using an Aether lens
let getLM l = 
    getL l <!> getM

/// Gets part of the Freya State within a Freya monad using a partial Aether lens
let getPLM l = 
    getPL l <!> getM

/// Sets part of the Freya State within a Freya monad using an Aether lens
let setLM l v = 
    setL l v |> modM

/// Sets part of the Freya State within a Freya monad using a partial Aether lens
let setPLM l v = 
    setPL l v |> modM

/// Modifies part of the Freya State within a Freya monad using an Aether lens
let modLM l f = 
    modL l f |> modM

/// Modifies part of the Freya State within a Freya monad using a partial Aether lens
let modPLM l f = 
    modPL l f |> modM