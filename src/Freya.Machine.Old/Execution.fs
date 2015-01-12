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
module Freya.Machine.Execution

open Freya.Core
open Freya.Core.Operators
open Freya.Pipeline

(* Execution *)

let private action a =
    core {
        do! a.Action
        do! addFreyaMachineExecutionRecord a.Id

        return a.Next }

let private decision d =
    core {
        let! result = d.Decision
        do! addFreyaMachineExecutionRecord d.Id

        match result with
        | true -> return d.True
        | _ -> return d.False }

let private handler (h: FreyaMachineHandlerNode) =
    core {
        do! addFreyaMachineExecutionRecord h.Id

        return h.Handler }

let private operation o =
    core {
        do! o.Operation
        do! addFreyaMachineExecutionRecord o.Id

        return o.Next }

let private traverse (graph: FreyaMachineGraph) =
    let rec eval from =
        core {
            match Map.find from graph with
            | ActionNode a -> return! action a >>= eval
            | DecisionNode d -> return! decision d >>= eval
            | HandlerNode h -> return! handler h
            | OperationNode o -> return! operation o >>= eval }

    eval Decisions.ServiceAvailable

(* Compilation *)

let compileFreyaMachine (machine: FreyaMachine) : Pipeline =
    let definition = snd (machine Map.empty)
    let graph = freyaMachineGraph definition
    let graphRecord = freyaMachineGraphRecord graph

    core {
        do! setFreyaMachineGraphRecord graphRecord
        do! setPLM definitionPLens definition
        do! traverse graph >>= represent

        return Halt }