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
module Freya.Machine.Recording

open Aether
open Aether.Operators
open Fleece
open Fleece.Operators
open Freya.Recorder

(* Keys *)

let [<Literal>] freyaMachineRecordKey =
    "machine"

(* Types *)

type FreyaMachineRecord =
    { Execution: FreyaMachineExecutionRecord
      Graph: FreyaMachineGraphRecord }

    static member ExecutionLens =
        (fun x -> x.Execution), (fun e x -> { x with Execution = e })

    static member GraphLens =
        (fun x -> x.Graph), (fun g x -> { x with Graph = g })

    static member ToJSON (x: FreyaMachineRecord) =
        jobj [
            "execution" .= x.Execution
            "graph" .= x.Graph ]

(* Graph *)

and FreyaMachineGraphRecord =
    { Nodes: FreyaMachineGraphNodeRecord list
      Edges: FreyaMachineGraphEdgeRecord list }

    static member ToJSON (x: FreyaMachineGraphRecord) =
        jobj [
            "nodes" .= x.Nodes
            "edges" .= x.Edges ]

and FreyaMachineGraphNodeRecord =
    { Id: string
      Type: string
      AllowOverride: bool
      HasOverride: bool }

    static member ToJSON (x: FreyaMachineGraphNodeRecord) =
        jobj [
            "id" .= x.Id
            "type" .= x.Type
            "allowOverride" .= x.AllowOverride
            "hasOverride" .= x.HasOverride ]

and FreyaMachineGraphEdgeRecord =
    { From: string
      To: string }

    static member ToJSON (x:FreyaMachineGraphEdgeRecord) =
        jobj [
            "from" .= x.From
            "to" .= x.To ]

(* Execution *)

and FreyaMachineExecutionRecord =
    { Nodes: FreyaMachineExecutionNodeRecord list }

    static member NodesLens =
        (fun x -> x.Nodes), (fun n x -> { x with FreyaMachineExecutionRecord.Nodes = n })

    static member ToJSON (x: FreyaMachineExecutionRecord) =
        jobj [
            "nodes" .= x.Nodes ]

and FreyaMachineExecutionNodeRecord =
    { Id: string }

    static member ToJSON (x: FreyaMachineExecutionNodeRecord) =
        jobj [
            "id" .= x.Id ]

(* Constructors *)

let private freyaMachineRecord =
    { Graph = 
        { Nodes = List.empty
          Edges = List.empty }
      Execution =
        { Nodes = List.empty } }

let private freyaMachineGraphNodeRecord id t allow has =
    { Id = id
      Type = t
      AllowOverride = allow
      HasOverride = has }

let private freyaMachineGraphEdgeRecord from t =
    { From = from
      To = t }

(* Lenses *)

let freyaMachineRecordPLens =
    recordDataPLens<FreyaMachineRecord> freyaMachineRecordKey

(* Functions *)

let private node id x =
    let node = freyaMachineGraphNodeRecord id

    match x with
    | ActionNode { Override = o } -> node "action" o.Allow o.Overridden
    | DecisionNode { Override = o } -> node "decision" o.Allow o.Overridden
    | HandlerNode { Override = o } -> node "handler" o.Allow o.Overridden
    | OperationNode _ -> node "operation" false false

let private edges id x =
    let edge = freyaMachineGraphEdgeRecord id

    match x with
    | ActionNode x -> [ edge x.Next ]
    | DecisionNode x -> [ edge x.True; edge x.False ]
    | HandlerNode _ -> []
    | OperationNode x -> [ edge x.Next ]

let internal graphRecord graph =
    let list = Map.toList graph
    let nodes = List.map (fun (k, v) -> node k v)
    let edges = List.map (fun (k, v) -> edges k v) >> List.concat

    { Nodes = nodes list
      Edges = edges list }

(* Recording *)

let initializeFreyaMachineRecord =
    updateRecord (setPL freyaMachineRecordPLens freyaMachineRecord)

let internal setFreyaMachineGraphRecord graph =
    updateRecord (setPL (     freyaMachineRecordPLens 
                         >?-> FreyaMachineRecord.GraphLens) graph)

let internal addFreyaMachineExecutionRecord id =
    updateRecord (modPL (     freyaMachineRecordPLens 
                         >?-> FreyaMachineRecord.ExecutionLens 
                         >?-> FreyaMachineExecutionRecord.NodesLens) 
                        (fun es -> es @ [ { Id = id } ]))