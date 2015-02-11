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
module internal Freya.Machine.Compilation

open Freya.Core
open Hekate

(* Types

   Types representing the result of specification, an Execution Graph, along with
   types to signify internal intermediary stages of compilation. *)

type ExecutionGraph =
    Graph<FreyaMachineNode, FreyaMachineOperation option, FreyaMachineEdge option>

type MetadataGraph =
    Graph<FreyaMachineNode, FreyaMachineOperationMetadata option, FreyaMachineEdge option>

type private SourceGraph =
    Graph<FreyaMachineNode, FreyaMachineCompiler option, FreyaMachineEdge option>

type Compilation =
    | Compiled of ExecutionGraph * MetadataGraph
    | Error of string

type private Ordering =
    | Ordered of FreyaMachineExtension list
    | Error

type private Extension =
    | Extended of SourceGraph
    | Error

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
    | _ -> Ordering.Error

let private order =
        Set.toList 
     >> graph 
     >> sort []

(* Application

   Functions to apply an ordered set of extensions to an existing
   BuildGraph instance, producing a new BuildGraph. *)

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

(* Compilation

   Functions to create a new ExecutionGraph by extending a default BuildGraph
   with the extensions contained within the specification (if possible after
   ordering), before compiling the BuildGraph to an Execution graph (by
   applying the compiler functions at nodes when present). *)

let private defaultSourceGraph : SourceGraph =
    Graph.create
        [ Start, None
          Finish, None ]
        [ Start, Finish, None ]

let private build config graph =
    let g1 = Graph.mapNodes (Option.map (fun (Compile n) -> n config)) graph
    let g2 = Graph.mapNodes (Option.map (fun (FreyaMachineCompilation.Compiled (o, _)) -> o)) g1
    let g3 = Graph.mapNodes (Option.map (fun (FreyaMachineCompilation.Compiled (_, m)) -> m)) g1

    g2, g3

let compile (spec: FreyaMachineSpecification) =
    match order spec.Extensions with
    | Ordered extensions ->
        match extend extensions defaultSourceGraph with
        | Extended source ->
            Compiled (build spec.Configuration source)
        | _ ->
            Compilation.Error ""
    | _ ->
        Compilation.Error ""