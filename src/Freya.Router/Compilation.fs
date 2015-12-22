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
module internal Freya.Router.Compilation

open Aether
open Aether.Operators
open Arachne.Uri.Template
open FParsec
open Freya.Core
open Hekate

(* Types

   Types representing the elements of a compiled Freya routing graph,
   modelling each aspect of the graph as a restrictive sum type. *)

type CompilationGraph =
    | Graph of Graph<CompilationKey, CompilationNode, CompilationEdge>

    static member graph_ =
        (fun (Graph g) -> g), (Graph)

 and CompilationKey =
    | Root
    | Key of string

 and CompilationNode =
    | Empty
    | Endpoints of CompilationEndpoint list

 and CompilationEndpoint =
    | Endpoint of int * FreyaRoutePredicate * FreyaPipeline

 and CompilationEdge =
    | Edge of Parser<UriTemplateData, unit>

(* Defaults

   Default values for common structures, in this case a default (empty)
   compilation graph for use as the basis in compilation. *)

let private defaultCompilationGraph =
    Graph (Graph.create [ Root, Empty ] [])

(* Lenses *)

let private compilationGraph_ =
    Lens.ofIsomorphism CompilationGraph.graph_

(* Patterns

   Active patterns used to discriminate while compiling a route,
   distinguishing between a part of the underlying URI Template
   which forms an intermediate node within the complete route,
   and the final element which should be represented within
   the graph as an endpoint (a node which has a non-empty list
   of Endpoint types). *)

let private (|Next|_|) =
    function | { Predicate = predicate
                 Specification = spec
                 Template = UriTemplate (part :: parts)
                 Pipeline = pipe } -> Some (part, { Predicate = predicate
                                                    Specification = spec
                                                    Template = UriTemplate (parts)
                                                    Pipeline = pipe })
             | _ -> None

let private (|Last|_|) =
    function | { Predicate = predicate
                 Specification = spec
                 Template = UriTemplate ([ part ])
                 Pipeline = pipe } -> Some (predicate, spec, part, pipe)
             | _ -> None

(* Modification

   Functions to modify aspects of the routing graph, chiefly
   to add routes to the graph (instances of FreyaRoute).

   A fairly simple recurse over the route, taking the head of
   the URI Template giving the route each time until exhausted. *)

let private composeKeys k1 k2 =
    match k1, k2 with
    | Key s1, Key s2 -> Key (s1 + s2)
    | _, Key s2 -> Key s2
    | Key s1, _ -> Key s1
    | _ -> Root

let private addNode key =
    Graph.addNode (key, Empty)

let private updateNode key precedence predicate pipe =
    Graph.mapNodes (fun key' node ->
        match key = key' with
        | true ->
            match node with
            | Empty -> Endpoints [ Endpoint (precedence, predicate, pipe) ]
            | Endpoints (endpoints) -> Endpoints (endpoints @ [ Endpoint (precedence, predicate, pipe) ])
        | _ ->
            node)

let private addEdge key1 key2 part graph =
    Graph.addEdge (key1, key2,
        Edge (UriTemplatePart.Matching.Match part)) graph

let rec private addRoute current graph (precedence, route) =
    match route with
    | Last (predicate, _, part, pipe) ->
        let last =
            composeKeys current (Key (part.ToString ()))

        let graph =
            ((fun graph ->
                (match Graph.containsNode last graph with
                 | false -> addNode last >> updateNode last precedence predicate pipe >> addEdge current last part
                 | _ -> updateNode last precedence predicate pipe) graph) ^% compilationGraph_) graph

        graph
    | Next (part, route) ->
        let next =
            composeKeys current (Key (part.ToString ()))

        let graph =
            ((fun graph ->
                (match Graph.containsNode next graph with
                 | false -> addNode next >> addEdge current next part
                 | _ -> id) graph) ^% compilationGraph_) graph

        addRoute next graph (precedence, route)
    | _ ->
        graph

(* Compilation

   A function to compile a list of raw FreyaRoute instances to
   an instance of a CompilationGraph, which can be executed
   directly (and hopefully efficiently). *)

let compile =
        List.mapi (fun precedence route -> precedence, route)
     >> List.fold (addRoute Root) defaultCompilationGraph