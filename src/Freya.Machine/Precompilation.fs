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
module internal Freya.Machine.Precompilation

open Aether
open Aether.Operators
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

type PrecompilationGraph =
    | Graph of Graph<FreyaMachineNode, FreyaMachineCompiler option, FreyaMachineEdge option>

    static member graph_ =
        (fun (Graph x) -> x), (fun x -> Graph (x))

type PrecompilationResult =
    | Precompilation of PrecompilationGraph
    | Error of string

(* Defaults *)

let private defaultPrecompilationGraph : PrecompilationGraph =
    PrecompilationGraph.Graph (
        Graph.create
            [ Start, None
              Finish, None ]
            [ Start, Finish, None ])

(* Lenses *)

let private precompilationGraph_ =
    Lens.ofIsomorphism PrecompilationGraph.graph_

(* Ordering

   Functions to order a set of machine dependencies. The sort/ordering
   is essentially a variation on Kahn's algorithm, which can be more
   simply expressed here as our graph library handles the removal of
   relevant edges as part of removing nodes.

   See [https://en.wikipedia.org/wiki/Topological_sorting] for details
   of topological sorting in general, and Kahn's algorithm in particular. *)

type private OrderingResult =
    | Ordering of FreyaMachineExtension list
    | Error of string

let private nodes =
    List.map (fun e -> e.Name, e)

let private edges =
        List.map (fun e -> e.Name, Set.toList e.Dependencies)
     >> List.map (fun (e, es) -> List.map (fun e' -> e', e, ()) es)
     >> List.concat

let private graph extensions =
    Graph.create (nodes extensions) (edges extensions)

let private independent graph =
    Graph.Nodes.toList graph
    |> List.tryFind (fun (v, _) -> Graph.Nodes.inwardDegree v graph = Some 0)
    |> Option.map (fun (v, l) -> l, Graph.Nodes.remove v graph)

let rec private sort extensions graph =
    match independent graph with
    | Some (e, g) -> sort (extensions @ [ e ]) g
    | _ when Graph.isEmpty graph -> Ordering extensions
    | _ -> Error "Extension Dependencies Cyclic"

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

type private Extension =
    | Extension of PrecompilationGraph
    | Error of string

let private applyOperation =
    function | AddNode (v, l) -> Graph.Nodes.add (v, l)
             | RemoveNode (v) -> Graph.Nodes.remove (v)
             | AddEdge (v1, v2, l) -> Graph.Edges.add (v1, v2, l)
             | RemoveEdge (v1, v2) -> Graph.Edges.remove (v1, v2)

let private applyExtension extension =
    flip (List.fold (flip applyOperation)) extension.Operations

let private applyExtensions =
    flip (List.fold (flip applyExtension))

let private extend extensions graph =
    Extension ((applyExtensions extensions ^% precompilationGraph_) graph)

(* Precompile

   The internally accessible function for precompiling a machine specification
   in to a complete source graph (or not, in the case of intermediate stage
   errors). *)

let precompile extensions =
    match order extensions with
    | Ordering extensions ->
        match extend extensions defaultPrecompilationGraph with
        | Extension graph -> Precompilation graph
        | Error e -> PrecompilationResult.Error e
    | OrderingResult.Error e ->
        PrecompilationResult.Error e
