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
module Freya.Machine.Http.Extension

open Freya.Machine

(* Operations

   The complete list of operations embodying the HTTP graph, composed
   of subsection listings. *)

let private operations =
    System.operations

(* Extension

   A concrete machine extension embodying the HTTP graph, generally
   expected to form the basis of most machine installations. With no
   dependencies, it is assumed that this extension will be the first
   modification of the default graph. *)

let http =
    { Name = "http"
      Dependencies = Set.empty
      Operations = operations }