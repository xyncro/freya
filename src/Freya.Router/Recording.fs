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
module Freya.Router.Recording

open Aether
open Aether.Operators
open Chiron
open Chiron.Operators
open Freya.Recorder
open Hekate

(* Keys *)

let [<Literal>] freyaRouterRecordKey =
    "router"

(* Types *)

type FreyaRouterRecord =
    { Graph: FreyaRouterGraphRecord
      Execution: FreyaRouterExecutionRecord }

    static member GraphLens =
        (fun x -> x.Graph), (fun g x -> { x with Graph = g })

    static member ExecutionLens =
        (fun x -> x.Execution), (fun e x -> { x with Execution = e })

    static member ToJson (x: FreyaRouterRecord) =
            Json.write "graph" x.Graph
         *> Json.write "execution" x.Execution

and FreyaRouterGraphRecord =
    { Nodes: FreyaRouterGraphNodeRecord list
      Edges: FreyaRouterGraphEdgeRecord list }

    static member NodesLens : Lens<FreyaRouterGraphRecord, FreyaRouterGraphNodeRecord list> =
        (fun x -> x.Nodes), (fun n x -> { x with Nodes = n })

    static member EdgesLens =
        (fun x -> x.Edges), (fun e x -> { x with Edges = e })

    static member ToJson (x: FreyaRouterGraphRecord) =
            Json.write "nodes" x.Nodes
         *> Json.write "edges" x.Edges

and FreyaRouterGraphNodeRecord =
    { Key: string
      Methods: string list }

    static member ToJson (x: FreyaRouterGraphNodeRecord) =
            Json.write "key" x.Key
         *> Json.write "methods" x.Methods

and FreyaRouterGraphEdgeRecord =
    { From: string
      To: string }

    static member ToJson (x: FreyaRouterGraphEdgeRecord) =
            Json.write "from" x.From
         *> Json.write "to" x.To

and FreyaRouterExecutionRecord =
    { Nodes: FreyaRouterExecutionNodeRecord list }

    static member NodesLens : Lens<FreyaRouterExecutionRecord, FreyaRouterExecutionNodeRecord list> =
        (fun x -> x.Nodes), (fun n x -> { x with Nodes = n })

    static member ToJson (x: FreyaRouterExecutionRecord) =
            Json.write "nodes" x.Nodes

and FreyaRouterExecutionNodeRecord =
    { Key: string
      Action: FreyaRouterExecutionAction }

    static member ToJson (x: FreyaRouterExecutionNodeRecord) =
            Json.write "key" x.Key
         *> Json.write "action" x.Action

and FreyaRouterExecutionAction =
    | Completion of FreyaRouterExecutionCompletionAction
    | Match of FreyaRouterExecutionMatchAction

    static member ToJson (x: FreyaRouterExecutionAction) =
        match x with
        | Completion x -> Json.write "completion" x
        | Match x -> Json.write "match" x

and FreyaRouterExecutionCompletionAction =
    | Success
    | Failure

    static member ToJson (x: FreyaRouterExecutionCompletionAction) =
        match x with
        | Success -> Json.write "result" "success"
        | Failure -> Json.write "result" "failure"

and FreyaRouterExecutionMatchAction =
    | Success
    | Failure

    static member ToJson (x: FreyaRouterExecutionMatchAction) =
        match x with
        | Success -> Json.write "result" "success"
        | Failure -> Json.write "result" "failure"

(* Construction *)

let internal createGraphRecord (Compilation.Graph graph) =
    { Nodes =
        Graph.nodes graph
        |> List.map (fun (v, l) ->
            match v, l with
            | Compilation.Root, _ ->
                { Key = "root"
                  Methods = [] }
            | Compilation.Key k, Compilation.Empty ->
                { Key = k
                  Methods = [] }
            | Compilation.Key k, Compilation.Endpoints es ->
                { Key = k
                  Methods = List.map (fun (Compilation.Endpoint (m, _)) -> m.ToString ()) es })
      Edges =
        Graph.edges graph
        |> List.map (fun (k1, k2, _) ->
            match k1, k2 with
            | Compilation.Root, Compilation.Key k ->
                { From = "root"
                  To = k }
            | Compilation.Key k1, Compilation.Key k2 ->
                { From = k1
                  To = k2 }
            | _ ->
                failwith "Edge Match Failure") }

(* Constructors *)

let private freyaRouterRecord () =
    { Graph =
        { Nodes = List.empty
          Edges = List.empty }
      Execution =
        { Nodes = List.empty } }

(* Lenses *)

let freyaRouterRecordPLens =
    FreyaRecorderRecord.KeyPLens<FreyaRouterRecord> freyaRouterRecordKey

let private graphPLens =
        freyaRouterRecordPLens
   >?-> FreyaRouterRecord.GraphLens

let private executionNodesPLens =
         freyaRouterRecordPLens
    >?-> FreyaRouterRecord.ExecutionLens
    >?-> FreyaRouterExecutionRecord.NodesLens

(* Recording *)

let private keyValue =
    function | Compilation.Root -> "root"
             | Compilation.Key k -> k

let internal recordGraph graph =
    FreyaRecorder.Current.map ((fun _ -> graph) ^?%= graphPLens)

let internal recordCompletionSuccess key =
    FreyaRecorder.Current.map ((fun nodes ->
        { Key = keyValue key
          Action = Completion FreyaRouterExecutionCompletionAction.Success } :: nodes) ^?%= executionNodesPLens)

let internal recordCompletionFailure key =
    FreyaRecorder.Current.map ((fun nodes ->
        { Key = keyValue key
          Action = Completion FreyaRouterExecutionCompletionAction.Failure } :: nodes) ^?%= executionNodesPLens)

let internal recordMatchSuccess key =
    FreyaRecorder.Current.map ((fun nodes ->
        { Key = keyValue key
          Action = Match Success } :: nodes) ^?%= executionNodesPLens)

let internal recordMatchFailure key =
    FreyaRecorder.Current.map ((fun nodes ->
        { Key = keyValue key
          Action = Match Failure } :: nodes) ^?%= executionNodesPLens)

(* Initialization *)

let initializeFreyaRouterRecord =
    FreyaRecorder.Current.map (freyaRouterRecord () ^?= freyaRouterRecordPLens)