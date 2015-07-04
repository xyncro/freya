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
module Freya.Machine.Inspector.Types

open Chiron
open Chiron.Operators
open Freya.Machine

(* Machine *)

type FreyaMachineInspection =
    { Graph: FreyaMachineGraphInspection
      Execution: FreyaMachineExecutionInspection }

    static member ToJson (x: FreyaMachineInspection) =
            Json.write "graph" x.Graph
         *> Json.write "execution" x.Execution

    static member internal OfRecord (record: Recording.FreyaMachineRecord) =
        { Graph =
            { Nodes =
                record.Graph.Nodes
                |> List.map (fun n ->
                    { Id = n.Id
                      Type = n.Type
                      Configurable = n.Configurable
                      Configured = n.Configured })
              Edges =
                record.Graph.Edges
                |> List.map (fun e ->
                    { From = e.From
                      To = e.To
                      Value = e.Value}) }
          Execution =
            { Nodes =
                record.Execution.Nodes
                |> List.map (fun n ->
                    { Id =
                        match n.Id with
                        | Start -> "start"
                        | Finish -> "finish"
                        | Operation x -> x }) } }

(* Graph *)

and FreyaMachineGraphInspection =
    { Nodes: FreyaMachineGraphNodeInspection list
      Edges: FreyaMachineGraphEdgeInspection list }

    static member ToJson (x: FreyaMachineGraphInspection) =
            Json.write "nodes" x.Nodes
         *> Json.write "edges" x.Edges

and FreyaMachineGraphNodeInspection =
    { Id: string
      Type: string
      Configurable: bool
      Configured: bool }

    static member ToJson (x: FreyaMachineGraphNodeInspection) =
            Json.write "id" x.Id
         *> Json.write "type" x.Type
         *> Json.write "configurable" x.Configurable
         *> Json.write "configured" x.Configured

and FreyaMachineGraphEdgeInspection =
    { From: string
      To: string
      Value: bool option }

    static member ToJson (x:FreyaMachineGraphEdgeInspection) =
            Json.write "from" x.From
         *> Json.write "to" x.To

(* Execution *)

and FreyaMachineExecutionInspection =
    { Nodes: FreyaMachineExecutionNodeInspection list }

    static member ToJson (x: FreyaMachineExecutionInspection) =
            Json.write "nodes" x.Nodes

and FreyaMachineExecutionNodeInspection =
    { Id: string }

    static member ToJson (x: FreyaMachineExecutionNodeInspection) =
            Json.write "id" x.Id