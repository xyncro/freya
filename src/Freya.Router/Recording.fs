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
open Freya.Recorder

(* Keys *)

let [<Literal>] freyaRouterRecordKey =
    "router"

(* Types *)

type FreyaRouterRecord =
    { Execution: FreyaRouterExecutionRecord }

    static member ExecutionLens =
        (fun x -> x.Execution), (fun e x -> { x with Execution = e })

    static member ToJson (x: FreyaRouterRecord) =
        Json.write "execution" x.Execution

and FreyaRouterExecutionRecord =
    { Nodes: FreyaRouterExecutionNodeRecord list }

    static member NodesLens =
        (fun x -> x.Nodes), (fun n x -> { x with Nodes = n })

    static member ToJson (x: FreyaRouterExecutionRecord) =
        Json.write "nodes" x.Nodes

and FreyaRouterExecutionNodeRecord =
    { Id: string }

    static member IdLens : Lens<FreyaRouterExecutionNodeRecord, string> =
        (fun x -> x.Id), (fun i x -> { x with Id = i })

    static member ToJson (x: FreyaRouterExecutionNodeRecord) =
        Json.write "id" x.Id

(* Defaults *)

let private defaultFreyaRouterRecord =
    { Execution =
        { Nodes = List.empty } }

(* Lenses *)

let freyaRouterRecordPLens =
    freyaRecordDataPLens<FreyaRouterRecord> freyaRouterRecordKey

let private nodesPLens =
         freyaRouterRecordPLens
    >?-> FreyaRouterRecord.ExecutionLens
    >?-> FreyaRouterExecutionRecord.NodesLens

(* Recording *)

let internal recordNode id =
    updateRecord ((fun nodes -> { Id = id } :: nodes) ^?%= nodesPLens)

(* Initialization *)

let initializeFreyaRouterRecord =
    updateRecord (defaultFreyaRouterRecord ^?= freyaRouterRecordPLens)