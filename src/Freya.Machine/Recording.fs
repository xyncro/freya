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
module internal Freya.Machine.Recording

open Aether.Operators
open Freya.Core
open Freya.Recorder
open Hekate

(* Errors *)

exception RecordingError of string

let private fail e =
    raise (RecordingError e)

(* Types *)

type FreyaMachineRecord =
    { Graph: FreyaMachineGraphRecord
      Execution: FreyaMachineExecutionRecord }

    static member Graph_ =
        (fun x -> x.Graph), (fun g x -> { x with Graph = g })

    static member Execution_ =
        (fun x -> x.Execution), (fun e x -> { x with Execution = e })

(* Graph *)

 and FreyaMachineGraphRecord =
    { Nodes: FreyaMachineGraphNodeRecord list
      Edges: FreyaMachineGraphEdgeRecord list }

 and FreyaMachineGraphNodeRecord =
    { Id: string
      Type: string
      Configurable: bool
      Configured: bool }

 and FreyaMachineGraphEdgeRecord =
    { From: string
      To: string
      Value: bool option }

(* Execution *)

 and FreyaMachineExecutionRecord =
    { Nodes: FreyaMachineExecutionNodeRecord list }

    static member Nodes_ =
        (fun x -> x.Nodes), (fun n x -> { x with FreyaMachineExecutionRecord.Nodes = n })

 and FreyaMachineExecutionNodeRecord =
    { Id: FreyaMachineNode }

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

let graphRecord (Compilation.Metadata meta) =
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

(* Recording *)

[<RequireQualifiedAccess>]
module Record =

    (* Lenses *)

    let private graph_ =
            Record.record_ "machine" 
         >- Option.mapLens FreyaMachineRecord.Graph_

    let private execution_ =
            Record.record_ "machine" 
         >- Option.mapLens FreyaMachineRecord.Execution_
         >? FreyaMachineExecutionRecord.Nodes_

    (* Functions *)

    let definition record =
        FreyaRecorder.Current.map (record ^= graph_)

    let execution node =
        FreyaRecorder.Current.map ((fun es -> es @ [ node ]) ^% execution_)