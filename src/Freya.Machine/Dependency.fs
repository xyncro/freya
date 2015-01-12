[<AutoOpen>]
module Freya.Machine.Dependency

open Aether

(* Types

   Types representing dependencies of string key, plus types supporting
   a dependency graph.

   Additional types expressing the result of an analysis of a preconstructed
   dependency graph, to find a simple valid ordering of dependencies.

   See later comments on Analysis. *)

type MachineDependency =
    | Dependency of MachineDependencyNode * Set<MachineDependencyNode>

and MachineDependencyGraph =
    { Nodes: Set<MachineDependencyNode>
      Edges: Set<MachineDependencyEdge> }

    static member NodesLens =
        (fun x -> x.Nodes), (fun n x -> { x with MachineDependencyGraph.Nodes = n })

    static member EdgesLens =
        (fun x -> x.Edges), (fun e x -> { x with MachineDependencyGraph.Edges = e })

and MachineDependencyNode =
    | DependencyNode of string

and MachineDependencyEdge =
    | DependencyEdge of MachineDependencyNode * MachineDependencyNode

type MachineDependencyGraphAnalysis =
    | Ordered of MachineDependencyNode list
    | Cyclic

(* Constructors

   Constructors for dependency graphs. *)

let machineDependencyGraph () : MachineDependencyGraph =
    { Nodes = Set.empty
      Edges = Set.empty }

(* Creation

   Functions for creating dependency graphs from a list
   of id and dependency list pairs. Forms a commonly modelled
   dependency graph as used for the basis of common topological sort
   algorithms.

   See [https://en.wikipedia.org/wiki/Topological_sorting] for details. *)

let private addNode (Dependency (x, _)) =
    modL MachineDependencyGraph.NodesLens (Set.add x)

let private addEdges (Dependency (x, xs)) =
    modL MachineDependencyGraph.EdgesLens 
        (Set.union 
            (Set.map (fun d -> DependencyEdge (x, d)) xs))

let createDependencyGraph =
    Set.fold (fun g e ->
        [ addNode e
          addEdges e ] |> List.fold (|>) g) (machineDependencyGraph ())

(* Analysis

   Dependency graph analysis attempts to sort dependencies in to
   topographically valid order based on dependencies. Kahn's
   algorithm is used to sort the nodes of the graph, leaving
   a dependency graph with an empty set of edges in the case of a
   valid dependency graph (one which excludes cyclic dependencies).

   See [https://en.wikipedia.org/wiki/Topological_sorting] for details. *)

let private hasIncomingEdges (g: MachineDependencyGraph) n =
    Set.exists (function | DependencyEdge (_, m) when n = m -> true
                         | _ -> false) g.Edges

let private edgesFromNode (g: MachineDependencyGraph) n =
    Set.filter (function | DependencyEdge (n', _) when n = n' -> true
                         | _ -> false) g.Edges

let private nodesToStartNodes g =
       Set.filter (hasIncomingEdges g >> not)
    >> Set.toList

let private edgesToStartNodes g =
       Set.map (fun (DependencyEdge (_, m)) -> m)
    >> nodesToStartNodes g

let rec private kahnsAlgorithm (g: MachineDependencyGraph) s l =
    match s with
    | [] ->
        match Set.isEmpty g.Edges with
        | true -> Ordered (List.rev l)
        | _ -> Cyclic
    | n :: ns ->
        let e = edgesFromNode g n
        let g = modL MachineDependencyGraph.EdgesLens (fun x -> x - e) g
        let s = ns @ edgesToStartNodes g e
        let l = l @ [ n ]

        kahnsAlgorithm g s l

let analyzeDependencyGraph graph =
    kahnsAlgorithm graph (nodesToStartNodes graph graph.Nodes) List.empty