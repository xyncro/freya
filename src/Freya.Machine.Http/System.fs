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
module internal Freya.Machine.Http.System

open Freya.Core
open Freya.Machine
open Freya.Machine.Operators

(* Decisions *)

let private serviceAvailable c =
    match config Decisions.ServiceAvailable c with
    | Some m -> configured, m
    | _ -> unconfigured, Freya.init true

(* Handlers *)

let private serviceUnavailable c =
    match config Handlers.ServiceUnavailable c with
    | Some m -> configured, m
    | _ -> unconfigured, Freya.init ()

(* Operations *)

let operations =
    [ Start                                 /*>       Finish

      Ref Decisions.ServiceAvailable        .|=       Binary serviceAvailable
      Ref Handlers.ServiceUnavailable       .|=       Unary serviceUnavailable
      
      Start                                 ..>       Ref Decisions.ServiceAvailable
      Ref Decisions.ServiceAvailable        .+>       Finish
      Ref Decisions.ServiceAvailable        .->       Ref Handlers.ServiceUnavailable
      Ref Handlers.ServiceUnavailable       ..>       Finish ]