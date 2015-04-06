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
module internal Freya.Machine.Reification

open Freya.Core
open Freya.Core.Operators
open Freya.Pipeline

(* Errors

   Reification may reasonably fail at runtime given the graph
   extension and verification involved. This is reported using a
   sprecifically defined reification exception, as this state is
   not meaningfully recoverable. It is likely that reification will
   occur early in the lifecycle of most programs, so while this is
   a runtime error, it is unlikely to be one which goes undiscovered
   for long.

   Errors in execution are dealt with separately.*)

exception ReificationError of string

let private fail e =
    raise (ReificationError e)

(* Run

   Running an execution with recording of metadata to the inspector.
   The graphs are effectively captured as a closure here, eliminating
   concerns about generation efficiency. *)

let private run exec record =
        recordGraph record
     *> execute exec
     *> halt

(* Reification

   Reify the specification of a machine into a pipeline function via
   precompilation/compilation/verification, producing both execution
   and metadata graphs. *)

let reify machine =
    let _, spec = machine defaultFreyaMachineSpecification

    match precompile spec.Extensions with
    | Precompilation precompilation ->
        match compile spec.Configuration precompilation with
        | Compilation (compilation, metadata) ->
            match verify compilation with
            | Verification compilation -> run compilation (createGraphRecord metadata)
            | Verification.Error e -> fail e
        | Compilation.Error e ->
            fail e
    | Precompilation.Error e ->
        fail e