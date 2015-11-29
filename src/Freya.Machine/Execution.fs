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
module internal Freya.Machine.Execution

open Aether
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

(* Types

   Types representing the outcome and traversal state of a machine
   execution. We currently only store the next node within the traversal
   state, but may expand this to capture more runtime data as part of
   the traversal state, potentially allowing dynamic optimization later. *)

type private ExecutionResult =
    | Success
    | Failure of string

type private Traversal =
    | Traversal of TraversalState

    static member State_ =
        (fun (Traversal x) -> x), (fun x -> Traversal x)

 and private TraversalState =
    | State of FreyaMachineNode

    static member Node_ =
        (fun (State x) -> x), (fun x -> State x)

(* Constructors

   Constructors for commonly created instances, in this case an initial
   state for a traversal starting from the provided node. *)

let private createTraversal node =
    Traversal (State node)

(* Lenses

   A lens from the traversal state to the current node, purely a simple
   isomorphism until the traversal state is potentially expanded. *)

let private compilationGraph_ =
        Lens.ofIsomorphism Compilation.CompilationGraph.Graph_

let private node_ =
        Lens.ofIsomorphism Traversal.State_
    >-> TraversalState.Node_

(* Patterns

   Patterns for matching the currently active node during traversal
   and extracting the relevant data to execute the next operation, in
   this case the current node in the case where the node is an
   operation. *)

let private (|Start|_|) =
    function | Traversal (State Start) -> Some ()
             | _ -> None

let private (|Operation|_|) =
    function | Traversal (State (Operation x)) -> Some (FreyaMachineNode.Operation x)
             | _ -> None

let private (|Finish|_|) =
    function | Traversal (State Finish) -> Some ()
             | _ -> None

(* Projection

   Functions to extract data from the compiled execution graph, finding
   a specific node, or the next node given a current node and a value
   which is expected to exist on an edge connecting the current node
   to the sought node. *)

let private tryFindNode node =
    Graph.tryFindNode node

let private tryFindNext node op =
        Graph.successors node
     >> function | Some edges ->
                    List.tryPick (fun edge ->
                        match edge with
                        | node', op' when op' = op -> Some node'
                        | _ -> None) edges
                 | _ ->
                    None

(* State

   Modifying the traversal state to reflect the current node within
   the traversal state. *)

let private progress node =
    node ^= node_

(* Traversal

   A traversal of the execution graph, running the unary or binary
   computations on relevant nodes and traversing paths indicated by
   the results of binary operations following binary edges where
   relevant. *)

let rec private traverse graph traversal =
    match traversal with
    | Operation node ->
        match tryFindNode node (graph ^. compilationGraph_) with
        | Some (_, Some (Unary op)) -> unary node op graph traversal
        | Some (_, Some (Binary op)) -> binary node op graph traversal
        | _ -> failwith ""
    | Start ->
        start graph traversal
    | Finish ->
        finish ()
    | _ ->
        failwith ""

 and private start graph traversal =
        Recording.Record.execution { Id = Start }
     *> ((fun _ -> tryFindNext Start None (graph ^. compilationGraph_)) <!> Freya.init ()
      >>= function | Some next -> traverse graph (progress next traversal)
                   | _ -> Freya.init (Failure ""))

 and private unary current op graph traversal =
        Recording.Record.execution { Id = current }
     *> ((fun _ -> tryFindNext current None (graph ^. compilationGraph_)) <!> op
      >>= function | Some next -> traverse graph (progress next traversal)
                   | _ -> failwith "")

 and private binary current op graph traversal =
        Recording.Record.execution { Id = current }
     *> ((fun x -> x, tryFindNext current (Some (Edge x)) (graph ^. compilationGraph_)) <!> op
      >>= function | _, Some next -> traverse graph (progress next traversal)
                   | _, _ -> failwith "")

 and private finish () =
        Recording.Record.execution { Id = Finish }
     *> Freya.init Success

(* Run

   Running an execution over a graph using a new traversal starting
   at the Start node as the initial traversal state. *)

let private run graph =
        createTraversal <!> Freya.init Start
    >>= traverse graph

(* Execute

   Function for executing a compiled graph, throwing on failure which
   may need reconsideration or a more generalised handling approach
   for genuinely runtime errors. *)

let execute graph =
        run graph
    >>= function | Success -> Freya.Pipeline.halt
                 | Failure e -> fail e