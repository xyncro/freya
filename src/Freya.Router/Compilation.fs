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
module Freya.Router.Compilation

open Aether
open Aether.Operators
open Freya.Pipeline
open Freya.Types.Uri.Template
open Hekate

(* Types

   Types representing the elements of a compiled Freya routing graph,
   modelling each aspect of the graph as a restrictive sum type.

   Lenses and isomorphisms are provided against the type definitions to
   make the resultant structure more tractable, and most code for both
   compilation and execution will require use of both Graph and Lens
   based functionality (provided by Hekate and Aether respectively). *)

type CompilationGraph =
    | Graph of Graph<CompilationKey, CompilationNode, CompilationEdge>

    static member GraphIso : Iso<CompilationGraph, Graph<CompilationKey, CompilationNode, CompilationEdge>> =
        (fun (Graph g) -> g),
        (fun g -> Graph g)

and CompilationKey =
    | Root
    | Key of string

    static member (+) (k1, k2) =
        match k1, k2 with
        | Key s1, Key s2 -> Key (s1 + s2)
        | _, Key s2 -> Key s2
        | Key s1, _ -> Key s1
        | _ -> Root

    override x.ToString () =
        (function | Root -> "root"
                  | Key s -> s) x

and CompilationNode =
    | Empty
    | Node of CompilationPart

    static member EmptyPIso : PIso<CompilationNode, unit> =
        (function | Empty -> Some () 
                  | _ -> None),
        (fun () -> Empty)

    static member NodePIso : PIso<CompilationNode, CompilationPart> =
        (function | Node n -> Some n 
                  | _ -> None),
        (fun n -> Node n)

    override x.ToString () =
        (function | Empty -> "empty"
                  | Node n -> sprintf "node %s" (n.ToString ())) x

and CompilationPart =
    | Part of UriTemplatePart * CompilationEndpoints

    static member PartLens : Lens<CompilationPart, UriTemplatePart> =
        (fun (Part (p, _)) -> p),
        (fun p (Part (_, es)) -> Part (p, es))

    static member EndpointsLens : Lens<CompilationPart, CompilationEndpoints> =
        (fun (Part (_, es)) -> es),
        (fun es (Part (p, _)) -> Part (p, es))

    override x.ToString () =
        (function | Part (p, _) -> sprintf "%s" (p.ToString ())) x

and CompilationEndpoints =
    | Endpoints of CompilationEndpoint list

    static member EndpointsIso : Iso<CompilationEndpoints, CompilationEndpoint list> =
        (fun (Endpoints es) -> es),
        (fun es -> Endpoints es)

and CompilationEndpoint =
    | Endpoint of FreyaRouteMethod * FreyaPipeline

    static member MethodLens : Lens<CompilationEndpoint, FreyaRouteMethod> =
        (fun (Endpoint (m, _)) -> m),
        (fun m (Endpoint (_, p)) -> Endpoint (m, p))

    static member PipelineLens : Lens<CompilationEndpoint, FreyaPipeline> =
        (fun (Endpoint (_, p)) -> p),
        (fun p (Endpoint (m, _)) -> Endpoint (m, p))

and CompilationEdge =
    | Edge of int

    static member EdgeIso : Iso<CompilationEdge, int> =
        (fun (Edge i) -> i),
        (fun i -> Edge i)

    override x.ToString () =
        (function | Edge i -> sprintf "edge %i" i) x

(* Defaults

   Default values for common structures, in this case a default (empty)
   compilation graph for use as the basis in compilation. *)

let private defaultCompilationGraph =
    Graph (Graph.create [ Root, Empty ] [])

(* Lenses

   Lenses used within compilation to provide access in to the complex
   data structure(s) used as a routing graph. *)

let private graphLens =
         idLens
    <--> CompilationGraph.GraphIso

let private endpointsPLens =
         idLens
    <-?> CompilationNode.NodePIso
    >?-> CompilationPart.EndpointsLens
    <?-> CompilationEndpoints.EndpointsIso

(* Patterns

   Active patterns used to discriminate while compiling a route,
   distinguishing between a part of the underlying URI Template
   which forms an intermediate node within the complete route,
   and the final element which should be represented within
   the graph as an endpoint (a node which has a non-empty list
   of Endpoint types). *)

let private (|Next|_|) =
    function | { Method = meth
                 Specification = spec
                 Template = UriTemplate (part :: parts)
                 Pipeline = pipe } -> Some (part, { Method = meth
                                                    Specification = spec
                                                    Template = UriTemplate (parts)
                                                    Pipeline = pipe })
             | _ -> None

let private (|Last|_|) =
    function | { Method = meth
                 Specification = spec
                 Template = UriTemplate (part :: [])
                 Pipeline = pipe } -> Some (meth, spec, part, pipe)
             | _ -> None

(* Modification

   Functions to modify aspects of the routing graph, chiefly
   to add routes to the graph (instances of FreyaRoute).

   A fairly simple recurse over the route, taking the head of
   the URI Template giving the route each time until exhausted. *)

let private addIntermediateNode key part =
    Graph.addNode (key,
        Node (Part (part, Endpoints [])))

let private addEndpointNode key meth pipe part =
    Graph.addNode (key,
        Node (Part (part, Endpoints [ Endpoint (meth, pipe) ])))

let private updateEndpointNode key meth pipe =
    Graph.mapNodes (fun k n ->
        match key = k with
        | true -> ((fun e -> e @ [ Endpoint (meth, pipe) ]) ^?%= endpointsPLens) n
        | _ -> n)

let private addEdge key1 key2 graph =
    Graph.addEdge (key1, key2,
        Edge (Option.get (Graph.outwardDegree key1 graph))) graph

let rec private addRoute current graph route =
    match route with
    | Last (meth, _, part, pipe) ->
        let last =
            current + Key (part.ToString ())
                
        let graph =
            ((fun graph ->
                (match Graph.containsNode last graph with
                 | false -> addEndpointNode last meth pipe part >> addEdge current last
                 | _ -> updateEndpointNode last meth pipe) graph) ^%= graphLens) graph

        graph
    | Next (part, route) ->
        let next =
            current + Key (part.ToString ())

        let graph =
            ((fun graph ->
                (match Graph.containsNode next graph with
                 | false -> addIntermediateNode next part >> addEdge current next
                 | _ -> id) graph) ^%= graphLens) graph

        addRoute next graph route
    | _ ->
        graph

(* Compilation

   A function to compile a list of raw FreyaRoute instances to
   an instance of a CompilationGraph, which can be executed
   directly (and hopefully efficiently). *)

let compile =
    List.fold (addRoute Root) defaultCompilationGraph