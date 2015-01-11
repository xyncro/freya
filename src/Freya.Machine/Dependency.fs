[<AutoOpen>]
module Freya.Machine.Dependency

open Aether

(* Types

   Types representing a dependency graph of entities keyed with a simple
   string identifier. Dependencies can be analysed to produce a
   dependency graph analysis, with a single valid result state, or other
   states representing unsatisifable sets of dependencies. *)

type DependencyGraph =
    { Nodes: Set<DependencyNode>
      Edges: Set<DependencyEdge> }

    static member NodesLens =
        (fun x -> x.Nodes), (fun n x -> { x with Nodes = n })

    static member EdgesLens =
        (fun x -> x.Edges), (fun e x -> { x with Edges = e })

and DependencyNode =
    | Node of string

and DependencyEdge =
    | Edge of string * string

type DependencyGraphAnalysis =
    | Ordered of DependencyNode list
    | Cyclic

(* Constructors

   Constructors for commonly used depdency types *)

let dependencyGraph () =
    { Nodes = Set.empty
      Edges = Set.empty }

(* Functions

   Functions for creating and analysing dependency graphs from a list
   of item * dependency list pairs.

   Dependency graph analysis attempts to sort dependencies in to
   topographically valid order based on dependencies. Kahn's
   algorithm is used to sort the nodes of the graph, leaving
   a dependency graph with an empty set of edges in the case of a
   valid dependency graph (one which excludes cyclic dependencies).

   See [https://en.wikipedia.org/wiki/Topological_sorting] for details. *)

(* Creation *)

let private addDependencyNode (e: string * _) =
    modL DependencyGraph.NodesLens (Set.add (Node (fst e)))

let private addDependencyEdges (e: string * string list) =
    modL DependencyGraph.EdgesLens 
        (Set.union 
            (set (List.map (fun d -> 
                    Edge ((fst e), d)) 
                    (snd e))))

let createDependencyGraph =
    List.fold (fun g e ->
        [ addDependencyNode e
          addDependencyEdges e ] |> List.fold (|>) g) (dependencyGraph ())

(* Analysis *)

let private hasIncomingEdges g (Node n) =
    Set.exists (function | Edge (_, m) when n = m -> true
                         | _ -> false) g.Edges

let private edgesFromNode g (Node n) =
    Set.filter (function | Edge (n', _) when n = n' -> true
                         | _ -> false) g.Edges

let private nodesToStartNodes g =
       Set.filter (hasIncomingEdges g >> not)
    >> Set.toList

let private edgesToStartNodes g =
       Set.map (fun (Edge (_, m)) -> Node m)
    >> nodesToStartNodes g

let rec private topologicalSort g s l =
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

        topologicalSort g s l

let analyzeDependencyGraph graph =
    topologicalSort graph (nodesToStartNodes graph graph.Nodes) List.empty