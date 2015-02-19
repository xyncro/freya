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
module internal Freya.Machine.Precompilation

open Freya.Core
open Hekate

(* Types

   Types representing the result of the precompilation of a machine graph,
   basically a source graph (produced by extending an empty source graph
   with the specified extensions - where possible - and the result of that
   precompilation, signifying success or failure.

   Additional types are used to represent the result states of intermediate
   stages, specifically dependency ordering of extensions, and the application
   of extensions to the source graph. *)

type Precompilation =
    | Precompiled of SourceGraph
    | Error of string

and SourceGraph =
    Graph<FreyaMachineNode, FreyaMachineCompiler option, FreyaMachineEdge option>

type private Ordering =
    | Ordered of FreyaMachineExtension list
    | Error of string

type private Extension =
    | Extended of SourceGraph
    | Error of string

(* Defaults *)

let private defaultSourceGraph : SourceGraph =
    Graph.create
        [ Start, None
          Finish, None ]
        [ Start, Finish, None ]

(* Ordering

   Functions to order a set of machine dependencies. The sort/ordering
   is essentially a variation on Kahn's algorithm, which can be more
   simply expressed here as our graph library handles the removal of
   relevant edges as part of removing nodes.

   See [https://en.wikipedia.org/wiki/Topological_sorting] for details
   of topological sorting in general, and Kahn's algorithm in particular. *)

let private nodes =
    List.map (fun e -> e.Name, e)

let private edges =
        List.map (fun e -> e.Name, Set.toList e.Dependencies)
     >> List.map (fun (e, es) -> List.map (fun e' -> e', e, ()) es)
     >> List.concat

let private graph extensions =
    Graph.create (nodes extensions) (edges extensions)

let private independent graph =
    Graph.nodes graph
    |> List.tryFind (fun (v, _) -> Graph.inwardDegree v graph = Some 0)
    |> Option.map (fun (v, l) -> l, Graph.removeNode v graph)

let rec private sort extensions graph =
    match independent graph with
    | Some (e, g) -> sort (e :: extensions) g
    | _ when Graph.isEmpty graph -> Ordered extensions
    | _ -> Ordering.Error "Extension Dependencies Cyclic"

let private order =
        Set.toList 
     >> graph 
     >> sort []

(* Extension

   Functions to apply an ordered set of extensions to an existing
   source graph instance, producing a new source graph. Future work should
   possibly extend this to find error cases earlier - possibly based around
   graph preconditions, but as we have the verification postcondition system
   it's not a high priority. *)

let private applyOperation =
    function | AddNode (v, l) -> Graph.addNode (v, l)
             | RemoveNode (v) -> Graph.removeNode (v)
             | AddEdge (v1, v2, l) -> Graph.addEdge (v1, v2, l)
             | RemoveEdge (v1, v2) -> Graph.removeEdge (v1, v2)

let private applyExtension extension =
    flip (List.fold (flip applyOperation)) extension.Operations

let private applyExtensions =
    flip (List.fold (flip applyExtension))

let private extend extensions g =
    Extended (applyExtensions extensions g)

(* Precompilation

   The internally accessible function for precompiling a machine specification
   in to a complete source graph (or not, in the case of intermediate stage
   errors). *)

let precompile spec =
    match order spec.Extensions with
    | Ordered extensions ->
        match extend extensions defaultSourceGraph with
        | Extended source ->
            Precompiled source
        | Extension.Error e ->
            Precompilation.Error e
    | Ordering.Error e ->
        Precompilation.Error e