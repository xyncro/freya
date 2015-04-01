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
module internal Freya.Router.Compilation

open Freya.Pipeline
open Freya.Types.Uri.Template
open Hekate

(* Types *)

type CompilationGraph =
    Graph<string, CompilationNode, CompilationEdge>

and CompilationNode =
    { EndPoints: (FreyaRouteMethod * FreyaPipeline) list }

    override x.ToString () =
        (function | { EndPoints = [] } -> sprintf "Node _"
                  | _ -> sprintf "Node [..]") x

and CompilationEdge =
    { Order: int
      Part: UriTemplatePart }

    override x.ToString () =
        (function | { Order = i
                      Part = p } -> 
                            sprintf "%i: Edge \"%s\"" i (p.ToString ())) x

(* Defaults *)

let private defaultCompilationGraph : CompilationGraph =
    Graph.create [ "", { EndPoints = [] } ] []

(* Compilation *)

let private ensureNode node endPoint graph =
    match Graph.containsNode node graph, endPoint with
    | false, Some endPoint -> Graph.addNode (node, { EndPoints = [ endPoint ]}) graph
    | false, _ -> Graph.addNode (node, { EndPoints = [] }) graph
    | _, Some endPoint ->
        Graph.mapNodes (fun v n ->
            match v = node with
            | true -> { n with EndPoints = n.EndPoints @ [ endPoint ] }
            | _ -> n) graph
    | _ -> graph

let private ensureEdge x y part graph =
    match Graph.containsEdge x y graph, Graph.outward x graph with
    | false, Some edges ->
        Graph.addEdge (x, y, { Order = edges.Length
                               Part = part }) graph
    | _ -> graph

let compile =
    let rec add current graph template =
        match template with
        | { Template = UriTemplate [] } ->
            graph
        | { Method = meth
            Match = matc
            Template = UriTemplate (part :: parts)
            Pipeline = pipeline } ->

            let next =
                current + UriTemplatePart.Format part

            let endPoint =
                match parts with
                | [] -> Some (meth, pipeline)
                | _ -> None

            let graph =
                graph
                |> ensureNode next endPoint
                |> ensureEdge current next part

            add next graph {
                Method = meth
                Match = matc
                Template = UriTemplate parts
                Pipeline = pipeline }

    List.fold (add "") defaultCompilationGraph