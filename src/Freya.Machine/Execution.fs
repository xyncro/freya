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
module internal Freya.Machine.Execution

open Aether.Operators
open Freya.Core
open Freya.Core.Operators
open Hekate

(* Errors

   Execution may (although should not) fail at runtime. Though
   the possibility of this for reasons captured by the type system
   should be small due to the verification system, we raise a specific
   error type in this instance. *)

exception ExecutionError of string

let private fail e =
    raise (ExecutionError e)

(* Operations

   Functions representing the execution and recording of monadic
   Machine operations, return the result of the operation when
   applicable (as in Binary operations). *)

let private start =
        execution "start"
     *> Freya.init None

let private finish =
        execution "finish"
     *> Freya.init ()

let private unary v operation =
        execution v
     *> operation
     *> Freya.init None

let private binary v operation =
        execution v
     *> operation
    >>= fun x -> Freya.init (Some (Edge x))

(* Patterns

   Patterns for matching the currently active node during traversal
   and extracting the relevant data to execute the next operation. *)

let private next node op =
        Graph.successors node
     >> Option.bind (List.tryFind (fun (_, op') -> op = op'))
     >> Option.map fst

let private (|Start|_|) =
    function | Some (Start, _) -> Some (flip (next FreyaMachineNode.Start))
             | _ -> None

let private (|Finish|_|) =
    function | Some (Finish, _) -> Some ()
             | _ -> None

let private (|Unary|_|) =
    function | Some (Operation key, Some (Unary op)) -> Some (flip (next (Operation key)), key, op)
             | _ -> None

let private (|Binary|_|) =
    function | Some (Operation key, Some (Binary op)) -> Some (flip (next (Operation key)), key, op)
             | _ -> None

(* Traversal

   Functions for executing against an execution graph, traversing the
   graph until either a Finish node is reached, or a node is
   unreachable, whether because the current node has no matching successors,
   or because the next node can't be found. *)

let rec private traverse graph node =
    match node with
    | Some node ->
        match Graph.tryFindNode node graph with
        | Unary (f, key, op) -> f graph <!> unary key op >>= traverse graph
        | Binary (f, key, op) -> f graph <!> binary key op >>= traverse graph
        | Start (f) -> f graph <!> start >>= traverse graph
        | Finish -> finish
        | _ -> fail "Node Not Matched"
    | _ ->
        fail "Node Not Found"

(* Execution

   Function for executing a compiled graph, throwing on failure which
   may need reconsideration or a more generalised handling approach
   for genuinely runtime errors. *)

let execute graph =
    traverse (graph ^. compilationGraphLens) (Some Start)