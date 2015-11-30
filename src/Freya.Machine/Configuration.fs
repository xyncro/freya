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

[<RequireQualifiedAccess>]
module Freya.Machine.Configuration

open Aether
open Aether.Operators
open Freya.Core

(* Lenses *)

let private config_<'a> key =
         FreyaMachineConfiguration.Data_
    >-?> key_ key
    <?-> box_<'a>

(* Configuration

   Functions supporting the mapping of configuration data to and from
   machine specifications, in a typed way (imposes a boxing and unboxing
   overhead, but is only used at reification time in general cases. *)

let tryGet<'a> key =
    flip (^?.) (config_<'a> key)

let tryGetOrElse key def =
    tryGet key >> Option.orElse def

let tryGetOrNone key =
    tryGet key >> function
    | Some x -> Freya.map Some x
    | None   -> Freya.init None

let set<'a> key =
    flip (^?=) (config_<'a> key)