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
open Chiron
open Chiron.Operators
open Hekate
open Freya.Recorder

(* Errors *)

exception RecordingError of string

let private fail e =
    raise (RecordingError e)

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

    static member ToJson (x: FreyaMachineRecord) =
            Json.write "execution" x.Execution
         *> Json.write "graph" x.Graph

(* Graph *)

and FreyaMachineGraphRecord =
    { Nodes: FreyaMachineGraphNodeRecord list
      Edges: FreyaMachineGraphEdgeRecord list }

    static member ToJson (x: FreyaMachineGraphRecord) =
            Json.write "nodes" x.Nodes
         *> Json.write "edges" x.Edges

and FreyaMachineGraphNodeRecord =
    { Id: string
      Type: string
      Configurable: bool
      Configured: bool }

    static member ToJson (x: FreyaMachineGraphNodeRecord) =
            Json.write "id" x.Id
         *> Json.write "type" x.Type
         *> Json.write "configurable" x.Configurable
         *> Json.write "configured" x.Configured

and FreyaMachineGraphEdgeRecord =
    { From: string
      To: string
      Value: bool option }

    static member ToJson (x:FreyaMachineGraphEdgeRecord) =
            Json.write "from" x.From
         *> Json.write "to" x.To

(* Execution *)

and FreyaMachineExecutionRecord =
    { Nodes: FreyaMachineExecutionNodeRecord list }

    static member NodesLens =
        (fun x -> x.Nodes), (fun n x -> { x with FreyaMachineExecutionRecord.Nodes = n })

    static member ToJson (x: FreyaMachineExecutionRecord) =
        Json.write "nodes" x.Nodes

and FreyaMachineExecutionNodeRecord =
    { Id: string }

    static member ToJson (x: FreyaMachineExecutionNodeRecord) =
        Json.write "id" x.Id

(* Construction *)

let private (|Name|) =
    function | Start -> "start"
             | Finish -> "finish"
             | Operation v -> v

let private (|StartOrFinish|_|) =
    function | Name n, None -> Some (n, n, false, false)
             | _ -> None

let private (|Unary|_|) =
    function | Name n, Some (c: FreyaMachineOperationMetadata) -> Some (n, "unary", c.Configurable, c.Configured)
             | _ -> None

let private (|Binary|_|) =
    function | Name n, Some (c: FreyaMachineOperationMetadata) -> Some (n, "binary", c.Configurable, c.Configured)
             | _ -> None

let internal createGraphRecord (Metadata meta) =
    { Nodes =
        Graph.nodes meta
        |> List.map (fun (v, l) ->
            let id, t, c1, c2 =
                match v, l with
                | StartOrFinish x 
                | Unary x 
                | Binary x -> x
                | _ -> fail "Recording Node Match Failure"
                    
            { Id = id
              Type = t
              Configurable = c1
              Configured = c2 })
      Edges =
        Graph.edges meta
        |> List.map (fun (Name v1, Name v2, l) ->
            { From =  v1
              To = v2
              Value = Option.map (fun (Edge l) -> l) l }) }

(* Defaults *)

let private defaultFreyaMachineRecord =
    { Execution =
        { Nodes = List.empty }
      Graph =
        { Nodes = List.empty
          Edges = List.empty } }

(* Lenses *)

let freyaMachineRecordPLens =
    freyaRecordDataPLens<FreyaMachineRecord> freyaMachineRecordKey

let private recordPLens =
         freyaMachineRecordPLens 
    >?-> FreyaMachineRecord.GraphLens

let private executionPLens =
         freyaMachineRecordPLens 
    >?-> FreyaMachineRecord.ExecutionLens 
    >?-> FreyaMachineExecutionRecord.NodesLens

(* Recording *)

let internal recordDefinition graph =
    updateRecord (graph ^?= recordPLens)

let internal recordExecution id =
    updateRecord ((fun es -> es @ [ { Id = id } ]) ^?%= executionPLens)

(* Initialization *)

let initializeFreyaMachineRecord =
    updateRecord (defaultFreyaMachineRecord ^?= freyaMachineRecordPLens)