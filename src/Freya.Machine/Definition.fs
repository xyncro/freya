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
module internal Freya.Machine.Definition

open Aether
open Aether.Operators

(* Aliases

   Convenience aliases for commonly used types for clarity
   and brevity. *)

type Graph =
    FreyaMachineGraph

type RefPair =
    FreyaMachineRefPair

(* Defaults

   Default instances of definition data types, in particular a general
   empty graph, consisting of the base state, a start and end node
   connected by a single unvalued edge. *)

let private defaultFreyaMachineGraph =
    { Graph.Nodes =
        Map.ofList [ 
            Start, None
            Finish, None ]
      Edges =
        Map.ofList [
            RefPair.Pair (Start, Finish), Value (None) ] }

(* Functions

   Functions for working with definition graphs within graph
   operations, not exposed externally, only asa composites within "safe"
   definition operation functions. *)

let private optionToBool =
    function | Some _ -> true
             | _ -> false

let private containsNode nodeRef =
    getPL (Graph.NodesLens >-?> mapPLens nodeRef) >> optionToBool

let private setNode nodeRef node =
    modL Graph.NodesLens (Map.add nodeRef (Some node))

let private unsetNode nodeRef =
    modL Graph.NodesLens (Map.remove nodeRef)

let private containsEdge sourceRef destRef =
    getPL (Graph.EdgesLens >-?> mapPLens (RefPair.Pair (sourceRef, destRef))) >> optionToBool

let private setEdge sourceRef destRef edge =
    modL Graph.EdgesLens (Map.add (RefPair.Pair (sourceRef, destRef)) edge)

let private unsetEdge sourceRef destRef =
    modL Graph.EdgesLens (Map.remove (RefPair.Pair (sourceRef, destRef)))

(* Operations

   Operations (functions of type DefinitionGraphOperation)
   on the DefinitionGraph type, plus functions for applying operations
   sequentially (folding over operations, with correct semantics for application
   under error states). *)

let addNewNode nodeRef node : FreyaMachineGraphOperation =
    function | g when not (containsNode nodeRef g) -> Choice1Of2 (setNode nodeRef node g)
             | _ -> Choice2Of2 (sprintf "Node [%A] Exists" nodeRef)

let removeExistingNode nodeRef : FreyaMachineGraphOperation =
    function | g when containsNode nodeRef g -> Choice1Of2 (unsetNode nodeRef g)
             | _ -> Choice2Of2 (sprintf "Node [%A] Does Not Exist" nodeRef)

let addNewEdge sourceRef destRef edge : FreyaMachineGraphOperation =
    function | g when not (containsEdge sourceRef destRef g) -> Choice1Of2 (setEdge sourceRef destRef edge g)
             | _ -> Choice2Of2 (sprintf "Edge [%A -> %A] Exists" sourceRef destRef)

let removeExistingEdge sourceRef destRef : FreyaMachineGraphOperation =
    function | g when containsEdge sourceRef destRef g -> Choice1Of2 (unsetEdge sourceRef destRef g)
             | _ -> Choice2Of2 (sprintf "Edge [%A -> %A] Does Not Exist" sourceRef destRef)

let private applyOperations operations graph =
    List.fold (fun result operation ->
        match result with
        | Choice1Of2 graph -> operation graph
        | Choice2Of2 e -> failwith e) graph operations

(* Creation *)

// TODO: Tidy this up!

let generateGraph spec =
    match orderExtensions spec.Extensions with
    | Ordered extensions ->
        let graph =
            List.fold (fun graph extension ->
                match graph with
                | Choice1Of2 graph -> applyOperations extension.Operations (Choice1Of2 graph)
                | Choice2Of2 e -> failwith e) (Choice1Of2 defaultFreyaMachineGraph) extensions

        match graph with
        | Choice1Of2 graph -> graph
        | Choice2Of2 e -> failwith e
    | Cyclic ->
        failwith "Extension Ordering Failed"