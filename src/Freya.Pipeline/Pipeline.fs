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

module Freya.Pipeline

open Freya.Core

(* Types *)

type FreyaPipeline =
    Freya<FreyaPipelineChoice>

and FreyaPipelineChoice =
    | Next
    | Halt

(* Helpers *)

let next : FreyaPipeline =
    Freya.returnM Next

let halt : FreyaPipeline =
    Freya.returnM Halt

(* Composition *)

let compose (p1: FreyaPipeline) (p2: FreyaPipeline) : FreyaPipeline =
    freya {
        let! pc = p1

        match pc with
        | Next -> return! p2
        | _ -> return Halt }


module Operators =

    let (>?=) p1 p2 : FreyaPipeline = 
        compose p1 p2
