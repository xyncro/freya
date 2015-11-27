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
module internal Freya.Router.Recording

open Aether.Operators
open Freya.Core
open Freya.Recorder
open Hekate

(* Types *)

type FreyaRouterRecord =
    { Graph: FreyaRouterGraphRecord
      Execution: FreyaRouterExecutionRecord }

    static member Graph_ =
        (fun x -> x.Graph), (fun g x -> { x with Graph = g })

    static member Execution_ =
        (fun x -> x.Execution), (fun e x -> { x with Execution = e })

 and FreyaRouterGraphRecord =
    { Nodes: FreyaRouterGraphNodeRecord list
      Edges: FreyaRouterGraphEdgeRecord list }

    static member Nodes_ =
        (fun x -> x.Nodes), (fun n x -> { x with FreyaRouterGraphRecord.Nodes = n })

    static member Edges_ =
        (fun x -> x.Edges), (fun e x -> { x with Edges = e })

 and FreyaRouterGraphNodeRecord =
    { Key: string
      Methods: string list }

 and FreyaRouterGraphEdgeRecord =
    { From: string
      To: string }

 and FreyaRouterExecutionRecord =
    { Nodes: FreyaRouterExecutionNodeRecord list }

    static member Nodes_ =
        (fun x -> x.Nodes), (fun n x -> { x with FreyaRouterExecutionRecord.Nodes = n })

 and FreyaRouterExecutionNodeRecord =
    { Key: string
      Action: FreyaRouterExecutionActionRecord }

 and FreyaRouterExecutionActionRecord =
    | Completion of FreyaRouterExecutionStatusRecord
    | Match of FreyaRouterExecutionStatusRecord

 and FreyaRouterExecutionStatusRecord =
    | Success
    | Failure

    override x.ToString () =
        match x with
        | Success -> "Success"
        | Failure -> "Failure"

(* Construction *)

let graphRecord (Compilation.Graph graph) =
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
                  Methods = List.map (fun (Compilation.Endpoint (_, m, _)) -> m.ToString ()) es })
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

(* Recording *)

[<RequireQualifiedAccess>]
module Record =

    (* Lenses *)

    let private graph_ =
            Record.Record_<FreyaRouterRecord> "router"
         >- Option.mapLens FreyaRouterRecord.Graph_

    let private executionNodes_ =
            Record.Record_ "router"
         >- Option.mapLens FreyaRouterRecord.Execution_
         >? FreyaRouterExecutionRecord.Nodes_

    (* Functions *)

    let private keyValue =
        function | Compilation.Root -> "root"
                 | Compilation.Key k -> k

    let graph graph =
        FreyaRecorder.Current.map ((fun _ -> graph) ^?% graph_)

    let completion status key =
        FreyaRecorder.Current.map ((fun nodes ->
            { Key = keyValue key
              Action = Completion status } :: nodes) ^?% executionNodes_)

    let match' status key =
        FreyaRecorder.Current.map ((fun nodes ->
            { Key = keyValue key
              Action = Match status } :: nodes) ^?% executionNodes_)