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
module internal Freya.Machine.Http.Prelude

open Freya.Core
open Freya.Machine

(* Configuration *)

let config<'a> =
    tryGetConfiguration<'a>

(* Configuration Metadata *)

let configured =
    { Configurable = true
      Configured = true }

let unconfigured =
    { Configurable = true
      Configured = false }

let unconfigurable =
    { Configurable = false
      Configured = false }

(* Decisions and Handlers *)

let decision key def c =
    match config key c with
    | Some m -> configured, m
    | _ -> unconfigured, Freya.init def

let handler key c =
    match config key c with
    | Some m -> configured, m
    | _ -> unconfigured, Freya.init ()

let operation f (_: FreyaMachineConfiguration) =
    unconfigurable, f
