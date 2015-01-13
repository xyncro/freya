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
module Freya.Machine.Reification

open Freya.Core.Operators
open Freya.Pipeline

(* Defaults

   Default instances of data types, in this case
   an empty machine specification with no existing configuration
   and no extensions (not a machine which will do much). *)

let private defaultFreyaMachineSpecification =
    { Configuration = 
        { Data = Map.empty }
      Extensions = Set.empty }

(* Reification

   Reify the specification of a machine into a pipeline function via
   compilation of the specification to a compilation map. *)

let reifyMachine (machine: FreyaMachine) =
    let spec = snd (machine defaultFreyaMachineSpecification)

    match compileSpecification spec with
    | Choice1Of2 compilation -> executeCompilation compilation *> halt
    | Choice2Of2 e -> failwith e