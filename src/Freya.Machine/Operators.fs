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

module Freya.Machine.Operators

(* Definition

   Infix operators for working with definition graphs, producing
   definition graph operations. Combined, these operators enable a
   moderately visual DSL for working with definition graphs. *)

let (>+) source dest =
    addNewEdge source dest (Value (Some true))

let (>-) source dest =
    addNewEdge source dest (Value (Some false))

let (>.) source dest =
    addNewEdge source dest (Value (None))

let (>/) =
    removeExistingEdge 

let (=.) id n =
    addNewNode id n

let (=/) id () =
    removeExistingNode id