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

[<AutoOpen>]
module Freya.Machine.Builder

(* Builder

   The Computation Expression builder to give Machine the declarative
   computation expression syntax for specifying Machine Definitions.
   Specific strongly typed custom operations are defined in
   Machine.Syntax.fs. *)

type FreyaMachineBuilder () =

    member __.Return _ : FreyaMachine =
        fun spec -> (), spec

    member __.ReturnFrom m : FreyaMachine = 
        m

    member __.Bind (m, k) : FreyaMachine = 
        m >> fun (result, definition) -> (k result) definition

    member x.Combine (m1, m2) : FreyaMachine = 
        x.Bind (m1, fun () -> m2)

    member x.Map (m, f) =
        x.Bind ((fun spec -> (), f spec), (fun _ -> x.ReturnFrom m))

let freyaMachine = FreyaMachineBuilder ()
