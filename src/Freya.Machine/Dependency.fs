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
module internal Freya.Machine.Dependency

open Aether

(* Types

   Types representing dependencies of string-based key, plus types supporting
   a dependency graph. Additional types expressing the result of an analysis 
   of a pre-constructed dependency graph, to find a simple valid ordering of
   dependencies.

   See later comments on Creation and Analysis. *)

type Dependency =
    | Dependency of DependencyRef * Set<DependencyRef>

and DependencyRef =
    | Ref of string

and DependencyRefPair =
    | Pair of DependencyRef * DependencyRef

and DependencyGraph =
    { Nodes: Set<DependencyRef>
      Edges: Set<DependencyRefPair> }

    static member NodesLens =
        (fun x -> x.Nodes), (fun n x -> { x with DependencyGraph.Nodes = n })

    static member EdgesLens =
        (fun x -> x.Edges), (fun e x -> { x with DependencyGraph.Edges = e })

type DependencyGraphOrdering =
    | Ordered of DependencyRef list
    | Cyclic

(* Defaults

   Default instances of dependency data types, in this case
   an empty dependency graph containing no nodes and edges. *)

let private defaultDependencyGraph =
    { Nodes = Set.empty
      Edges = Set.empty }

(* Creation

   Functions for creating dependency graphs from a list
   of id and dependency list pairs. Forms a commonly modelled
   dependency graph as used for the basis of common topological sort
   algorithms.

   See [https://en.wikipedia.org/wiki/Topological_sorting] for details. *)

let private addNode (Dependency (x, _)) =
    modL DependencyGraph.NodesLens (Set.add x)

let private addEdges (Dependency (x, xs)) =
    modL DependencyGraph.EdgesLens (Set.union (Set.map (fun d -> Pair (x, d)) xs))

let createDependencyGraph =
    Set.fold (fun g e ->
        [ addNode e
          addEdges e ] |> List.fold (|>) g) defaultDependencyGraph

(* Ordering

   Dependency graph ordering attempts to sort dependencies in to
   topographically valid order. Kahn's algorithm is used to sort the
   nodes of the graph, leaving a dependency graph with an empty set
   of edges in the case of a valid dependency graph (one which
   excludes cyclic dependencies).

   See [https://en.wikipedia.org/wiki/Topological_sorting] for details. *)

let private hasIncomingEdges (g: DependencyGraph) n =
    Set.exists (function | Pair (_, m) when n = m -> true
                         | _ -> false) g.Edges

let private edgesFromNode (g: DependencyGraph) n =
    Set.filter (function | Pair (n', _) when n = n' -> true
                         | _ -> false) g.Edges

let private nodesToStartNodes g =
       Set.filter (hasIncomingEdges g >> not)
    >> Set.toList

let private edgesToStartNodes g =
       Set.map (fun (Pair (_, m)) -> m)
    >> nodesToStartNodes g

let rec private sort (g: DependencyGraph) s l =
    match s with
    | [] ->
        match Set.isEmpty g.Edges with
        | true -> Ordered (List.rev l)
        | _ -> Cyclic
    | n :: ns ->
        let e = edgesFromNode g n
        let g = modL DependencyGraph.EdgesLens (fun x -> x - e) g
        let s = ns @ edgesToStartNodes g e
        let l = l @ [ n ]

        sort g s l

let orderDependencyGraph graph =
    sort graph (nodesToStartNodes graph graph.Nodes) List.empty