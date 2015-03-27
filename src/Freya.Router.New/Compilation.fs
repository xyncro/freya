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
//----------------------------------------------------------------------------

[<AutoOpen>]
module Freya.Router.Compilation

open Freya.Types.Uri.Template
open Hekate

(* Types *)

type CompilationGraph =
    Graph<string, CompilationNode, CompilationEdge>

and CompilationNode =
    { Value: int option }

    override x.ToString () =
        (function | { Value = Some i } -> sprintf "Node %i" i
                  | _ -> sprintf "Node _") x

and CompilationEdge =
    { Order: int
      Part: UriTemplatePart }

    override x.ToString () =
        (function | { Order = i
                      Part = p } -> 
                            sprintf "%i: Edge \"%s\"" i (p.ToString ())) x

(* Defaults *)

let internal defaultCompilationGraph : CompilationGraph =
    Graph.create [ "", { Value = None } ] []

(* Compilation *)

let private node (parts: _ list) value =
    { Value =
        (function | 0 -> Some value
                  | _ -> None) parts.Length }

let private edge current part graph =
    { Order =
        graph
        |> Graph.outwardDegree current
        |> Option.get
      Part = part }

let private insert current next node edge =
        Graph.addNode (next, node)
     >> Graph.addEdge (current, next, edge)

let compile =
    let rec add current graph template =
        match template with
        | UriTemplate [], _ ->
            graph
        | UriTemplate (p :: ps), i ->
            let node = node ps i
            let edge = edge current p graph
            let next = current + UriTemplatePart.Format p
            let graph = insert current next node edge graph

            add next graph (UriTemplate ps, i)

    List.fold (add "") defaultCompilationGraph