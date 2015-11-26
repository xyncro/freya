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
module internal Freya.Machine.Verification

(* NOTE: This verification system is rather basic at the moment
   and everything down to the stylistic choice of design (perhaps
   *especially* the stylistic choice of design!) is open to
   reconsideration and suggestion.

   Ideally a fairly declarative system of constraints might be
   designed, which would possibly have relevance to being ported
   back in to the core Hekate library. *)

open Aether
open Aether.Operators
open Freya.Core
open Hekate

(* Types

   Type supporting the verification of execution graphs, by a set of
   graph postconditions, ensuring some level of runtime safety when
   executing a verified graph. *)

type VerificationResult =
    | Verification of Compilation.CompilationGraph
    | Error of string

(* Projections

   Functions and a type signature to represent various projections
   over a compilation graph, returning a specific set of nodes,
   which may then be checked for properties using specific assertions. *)

type private Projection =
    | Projection of (Compilation.CompilationGraph -> FreyaMachineNode list)

let private startNodes =
    Projection (
            flip (^.) (id_ >- Compilation.CompilationGraph.Graph_)
         >> Graph.nodes
         >> List.choose (function | (Start, _) -> Some Start
                                  | _ -> None))

let private finishNodes =
    Projection (
            flip (^.) (id_ >- Compilation.CompilationGraph.Graph_)
         >> Graph.nodes
         >> List.choose (function | (Finish, _) -> Some Finish
                                  | _ -> None))

let private unaryNodes =
    Projection (
            flip (^.) (id_ >- Compilation.CompilationGraph.Graph_)
         >> Graph.nodes
         >> List.choose (function | (v, Some (Unary _)) -> Some v
                                  | _ -> None))

let private binaryNodes =
    Projection (
            flip (^.) (id_ >- Compilation.CompilationGraph.Graph_)
         >> Graph.nodes
         >> List.choose (function | (v, Some (Binary _)) -> Some v
                                  | _ -> None))

(* Assertions

   Assertion types and functions, where an assertion is an
   individual assertion, and should be applied to each node within
   a list of nodes, or a group assertion, and applies to the
   list of nodes as a whole (for instance, checking the size
   of a group of nodes). *)

type private Assertion =
    | Individual of (Compilation.CompilationGraph -> FreyaMachineNode -> bool)
    | Group of (Compilation.CompilationGraph -> FreyaMachineNode list -> bool)

let private size i =
    Group (fun _ nodes -> List.length nodes = i)

let private haveNoSuccessors =
    Individual (fun (Compilation.CompilationGraph.Graph graph) node ->
        match Graph.successors node graph with
        | Some [] -> true
        | _ -> false)

let private haveUnarySuccessors =
    Individual (fun (Compilation.CompilationGraph.Graph graph) node ->
        match Graph.successors node graph with
        | Some s when List.length s = 1 -> true
        | _ -> false)

let private haveBinarySuccessors =
    Individual (fun (Compilation.CompilationGraph.Graph graph) node ->
        match Graph.successors node graph with
        | Some s when List.length s = 2 -> true
        | _ -> false)

(* Constraints

   Conceptual constraints and functions to check constraints,
   where a constraint is a combination of a projection and an
   assertion to be applied to the results of the projection, along
   with a string to return as an error in case of constraint
   failure.

   Constraint groups are provided as a logical grouping of constraints
   which may relate to specific aspects of the compilation graph. *)

type private ConstraintGroup =
    | Constraints of Constraint list

and private Constraint =
    | Constraint of Projection * Assertion * string

let private check graph (Constraint (Projection projection, assertion, error)) =
    let nodes = projection graph

    match assertion with
    | Individual assertion ->
        List.fold (fun state node ->
            match state with
            | Some error ->
                Some error
            | _ ->
                match assertion graph node with
                | true -> None
                | _ -> Some error) None nodes
    | Group assertion ->
        match assertion graph nodes with
        | true -> None
        | _ -> Some error

let private checkGroup graph (Constraints constraints) =
    List.fold (fun state con ->
        match state with
        | Some error -> Some error
        | _ -> check graph con) None constraints

let private checkGroups graph =
    List.fold (fun state group ->
        match state with
        | Some error -> Some error
        | _ -> checkGroup graph group) None

(* Invariants

   Constraint groups forming invariants which should hold for a
   valid compilation graph, relating to specific properties of that
   graph.
   
   The invariants are currently fairly basic, and could be extended
   at some point when needed. Effectively the constraints form a kind of
   runtime type checking for valid structure in the absence of a stronger
   type level mechanism for doing so (i.e. dependent types or similar). *)

let private start =
    Constraints [
        Constraint (startNodes, size 1, "Start Node Must Exist")
        Constraint (startNodes, haveUnarySuccessors, "Start Node Must Have One Successor") ]

let private finish =
    Constraints [
        Constraint (finishNodes, size 1, "Finish Node Must Exist")
        Constraint (finishNodes, haveNoSuccessors, "Finish Node Must Have No Successors") ]

let private unary =
    Constraints [
        Constraint (unaryNodes, haveUnarySuccessors, "Unary Nodes Must Have One Successor") ]
        
let private binary =
    Constraints [
        Constraint (binaryNodes, haveBinarySuccessors, "Binary Nodes Must Have Two Successors") ]

let private invariants =
    [ start
      finish
      unary
      binary ]

(* Verify *)

let verify graph =
        checkGroups graph invariants
     |> function | Some error -> Error error
                 | _ -> Verification graph