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

(* Extension

   Infix operators for working with graph extensions, as part
   of the general model of machine extensions. *)

let (=.) id n =
    AddNode (id, n)

let (=/) id () =
    RemoveNode (id)

let (>+) source dest =
    AddEdge (source, dest, Some (Edge true))

let (>-) source dest =
    AddEdge (source, dest, Some (Edge false))

let (>.) source dest =
    AddEdge (source, dest, None)

let (>/) source dest =
    RemoveEdge (source, dest)