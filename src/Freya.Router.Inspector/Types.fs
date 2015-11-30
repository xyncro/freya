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
module Freya.Router.Inspector.Types

open Chiron
open Chiron.Operators
open Freya.Router

(* Router *)

type FreyaRouterInspection =
    { Graph: FreyaRouterGraphInspection
      Execution: FreyaRouterExecutionInspection }

    static member ToJson (x: FreyaRouterInspection) =
            Json.write "graph" x.Graph
         *> Json.write "execution" x.Execution

    static member internal OfRecord (record: Recording.FreyaRouterRecord) =
        { Graph =
            { Nodes =
                record.Graph.Nodes
                |> List.map (fun n ->
                    { Key = n.Key
                      Methods = n.Methods })
              Edges =
                record.Graph.Edges
                |> List.map (fun e ->
                    { From = e.From
                      To = e.To }) }
          Execution =
            { Nodes =
                record.Execution.Nodes
                |> List.map (fun n ->
                    { Key = n.Key
                      Action =
                        match n.Action with
                        | Recording.Completion Recording.Success -> Completion Success
                        | Recording.Completion Recording.Failure -> Completion Failure
                        | Recording.Match Recording.Success -> Match Success
                        | Recording.Match Recording.Failure -> Match Failure }) } }

(* Graph *)

and FreyaRouterGraphInspection =
    { Nodes: FreyaRouterGraphNodeInspection list
      Edges: FreyaRouterGraphEdgeInspection list }

    static member ToJson (x: FreyaRouterGraphInspection) =
            Json.write "nodes" x.Nodes
         *> Json.write "edges" x.Edges

and FreyaRouterGraphNodeInspection =
    { Key: string
      Methods: string list }

    static member ToJson (x: FreyaRouterGraphNodeInspection) =
            Json.write "key" x.Key
         *> Json.write "methods" x.Methods

and FreyaRouterGraphEdgeInspection =
    { From: string
      To: string }

    static member ToJson (x: FreyaRouterGraphEdgeInspection) =
            Json.write "from" x.From
         *> Json.write "to" x.To

(* Execution *)

and FreyaRouterExecutionInspection =
    { Nodes: FreyaRouterExecutionNodeInspection list }

    static member ToJson (x: FreyaRouterExecutionInspection) =
            Json.write "nodes" x.Nodes

and FreyaRouterExecutionNodeInspection =
    { Key: string
      Action: FreyaRouterExecutionActionInspection }

    static member ToJson (x: FreyaRouterExecutionNodeInspection) =
            Json.write "key" x.Key
         *> Json.write "action" x.Action

and FreyaRouterExecutionActionInspection =
    | Completion of FreyaRouterExecutionStatusInspection
    | Match of FreyaRouterExecutionStatusInspection

    static member ToJson (x: FreyaRouterExecutionActionInspection) =
        match x with
        | Completion x -> Json.write "completion" x
        | Match x -> Json.write "match" x

and FreyaRouterExecutionStatusInspection =
    | Success
    | Failure

    static member ToJson (x: FreyaRouterExecutionStatusInspection) =
        match x with
        | Success -> Json.write "result" "success"
        | Failure -> Json.write "result" "failure"