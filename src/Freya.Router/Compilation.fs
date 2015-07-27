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

    static member Graph_ =
        (fun (Graph g) -> g), (fun g -> Graph g)

and CompilationKey =
    | Root
    | Key of string

    override x.ToString () =
        match x with
        | Root -> "<key>"
        | Key x -> sprintf "<key %s>" x

and CompilationNode =
    | Empty
    | Endpoints of CompilationEndpoint list

    override x.ToString () =
        match x with
        | Empty -> "<node>"
        | Endpoints x -> sprintf "<node %A>" x

and CompilationEndpoint =
    | Endpoint of int * FreyaRouteMethod * FreyaPipeline

    override x.ToString () =
        match x with
        | Endpoint (i, m, _) -> sprintf "<endpoint %i:%A>" i m

and CompilationEdge =
    | Edge of Parser<UriTemplateData, unit>

    override x.ToString () =
        "<edge>"

(* Defaults

   Default values for common structures, in this case a default (empty)
   compilation graph for use as the basis in compilation. *)

let private defaultCompilationGraph =
    Graph (Graph.create [ Root, Empty ] [])

(* Lenses *)

let private compilationGraph_ =
        idLens
   <--> CompilationGraph.Graph_

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

let private composeKeys k1 k2 =
    match k1, k2 with
    | Key s1, Key s2 -> Key (s1 + s2)
    | _, Key s2 -> Key s2
    | Key s1, _ -> Key s1
    | _ -> Root

let private addNode key =
    Graph.addNode (key, Empty)

let private updateNode key precedence meth pipe =
    Graph.mapNodes (fun key' node ->
        match key = key' with
        | true ->
            match node with
            | Empty -> Endpoints [ Endpoint (precedence, meth, pipe) ]
            | Endpoints (endpoints) -> Endpoints (endpoints @ [ Endpoint (precedence, meth, pipe) ])
        | _ ->
            node)

let private addEdge key1 key2 part graph =
    Graph.addEdge (key1, key2,
        Edge (UriTemplatePart.Matching.Match part)) graph

let rec private addRoute current graph (precedence, route) =
    match route with
    | Last (meth, _, part, pipe) ->
        let last =
            composeKeys current (Key (part.ToString ()))

        let graph =
            ((fun graph ->
                (match Graph.containsNode last graph with
                 | false -> addNode last >> updateNode last precedence meth pipe >> addEdge current last part
                 | _ -> updateNode last precedence meth pipe) graph) ^%= compilationGraph_) graph

        graph
    | Next (part, route) ->
        let next =
            composeKeys current (Key (part.ToString ()))

        let graph =
            ((fun graph ->
                (match Graph.containsNode next graph with
                 | false -> addNode next >> addEdge current next part
                 | _ -> id) graph) ^%= compilationGraph_) graph

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