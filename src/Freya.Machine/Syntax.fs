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
module Freya.Machine.Syntax

open Aether.Operators

(* Builder

   The Computation Expression builder to give Machine the declarative
   computation expression syntax for specifying Machine Definitions.
   Specific strongly typed custom operations are defined in Syntax.fs. *)

type FreyaMachineBuilder () =

    member __.Return _ =
        FreyaMachine (fun spec -> (), spec)

    member __.ReturnFrom m =
        m

    member __.Bind (m, k) =
        FreyaMachine (fun spec ->
            let (FreyaMachine m') = m
            let (FreyaMachine k') = k ()

            (), snd (k' (snd (m' spec))))

    member x.Combine (m1, m2) =
        x.Bind (m1, fun () -> m2)

    member x.Map (m, f) =
        x.Bind (FreyaMachine (fun spec -> (), f spec), (fun _ -> m))

let freyaMachine =
    FreyaMachineBuilder ()

type FreyaMachineBuilder with

    /// Includes an existing Freya Machine expression, effectively inheriting the
    /// properties of the existing expression.
    [<CustomOperation ("including", MaintainsVariableSpaceUsingBind = true)>]
    member x.Including (m, definition) = 
        x.Combine (m, definition)

    /// Uses a Freya Machine Extension, used to implement different request handling
    /// approaches. Multiple Extensions can be combined to form a suitable processing
    /// pipeline.
    [<CustomOperation ("using", MaintainsVariableSpaceUsingBind = true)>]
    member x.Using (m, extension) =
        x.Map (m, Set.add extension ^% FreyaMachineSpecification.Extensions_)