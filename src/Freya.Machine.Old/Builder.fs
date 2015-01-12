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
module Freya.Machine.Builder

open Aether

(* Builder

   The Computation Expression builder to give Machine the declarative
   computation expression syntax for specifying Machine Definitions.
   Specific strongly typed custom operations are defined in
   Machine.Syntax.fs. *)

type FreyaMachineBuilder () =

    member __.Return _ : FreyaMachine =
        fun definition -> (), definition

    member __.ReturnFrom machine : FreyaMachine = 
        machine

    member __.Bind (m, k) : FreyaMachine = 
        m >> fun (result, definition) -> (k result) definition

    member x.Combine (m1, m2) : FreyaMachine = 
        x.Bind (m1, fun () -> m2)

    member internal x.Set (r, lens, value) = 
        x.Bind ((fun res -> (), setPL lens value res), fun _ -> x.ReturnFrom r)

let machine = FreyaMachineBuilder ()
