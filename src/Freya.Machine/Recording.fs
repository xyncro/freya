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
module Freya.Machine.Recording

open Aether
open Aether.Operators
open Fleece
open Fleece.Operators
open Freya.Recorder

(* Aliases

   Convenience type aliases when we have some more specific unions
   etc. in scope, in this case clashes between machine level refs
   and compilation refs. *)

type Ref =
    FreyaMachineRef


(* Keys *)

let [<Literal>] freyaMachineRecordKey =
    "machine"

(* Helpers *)

let refString =
    function | Ref.Start -> "start"
             | Ref.Ref x -> x
             | Ref.Finish -> "finish"

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
    { Id: Ref
      Type: string
      Configurable: bool
      Configured: bool }

    static member ToJSON (x: FreyaMachineGraphNodeRecord) =
        jobj [
            "id" .= refString x.Id
            "type" .= x.Type
            "configurable" .= x.Configurable
            "configured" .= x.Configured ]

and FreyaMachineGraphEdgeRecord =
    { From: Ref
      To: Ref }

    static member ToJSON (x:FreyaMachineGraphEdgeRecord) =
        jobj [
            "from" .= refString x.From
            "to" .= refString x.To ]

(* Execution *)

and FreyaMachineExecutionRecord =
    { Nodes: FreyaMachineExecutionNodeRecord list }

    static member NodesLens =
        (fun x -> x.Nodes), (fun n x -> { x with FreyaMachineExecutionRecord.Nodes = n })

    static member ToJSON (x: FreyaMachineExecutionRecord) =
        jobj [
            "nodes" .= x.Nodes ]

and FreyaMachineExecutionNodeRecord =
    { Id: Ref }

    static member ToJSON (x: FreyaMachineExecutionNodeRecord) =
        jobj [
            "id" .= refString x.Id ]

(* Constructors *)

let private freyaMachineRecord =
    { Graph = 
        { Nodes = List.empty
          Edges = List.empty }
      Execution =
        { Nodes = List.empty } }

let private freyaMachineGraphNodeRecord id t c1 c2 =
    { Id = id
      Type = t
      Configurable = c1
      Configured = c2 }

let private freyaMachineGraphEdgeRecord from t =
    { From = from
      To = t }

let internal freyaMachineGraphRecord (map: CompilationMap) =
    let list = 
        Map.toList map

    let node =
        freyaMachineGraphNodeRecord

    let edge =
        freyaMachineGraphEdgeRecord

    let nodes =
        List.map (fun (k, v) ->
            match k, v with
            | Ref.Start, Start _ -> node Ref.Start "start" false false
            | Ref.Ref r, Unary x -> node (Ref.Ref r) "unary" x.Configuration.Configurable x.Configuration.Configured
            | Ref.Ref r, Binary x -> node (Ref.Ref r) "binary" x.Configuration.Configurable x.Configuration.Configured
            | Ref.Finish, _ -> node Ref.Finish "finish" false false
            | _ -> failwith "invalid compilation") list

    let edges = 
        List.map (fun (k, v) ->
            match k, v with
            | Ref.Start, Start x -> [ edge Ref.Start x.Next ]
            | Ref.Ref r, Unary x -> [ edge (Ref.Ref r) x.Next ]
            | Ref.Ref r, Binary x -> [ edge (Ref.Ref r) x.True; edge (Ref.Ref r) x.False ]
            | _ -> []) list |> List.concat

    { FreyaMachineGraphRecord.Nodes = nodes
      Edges = edges }

(* Lenses *)

let freyaMachineRecordPLens =
    freyaRecordDataPLens<FreyaMachineRecord> freyaMachineRecordKey

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