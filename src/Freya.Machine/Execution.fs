[<AutoOpen>]
module Freya.Machine.Execution

open Freya.Core
open Freya.Core.Operators
open Freya.Pipeline

(* Execution *)

let private action a =
    freya {
        do! a.Action
        do! executionFreyaMachineR a.Id

        return a.Next }

let private decision d =
    freya {
        let! result = d.Decision
        do! executionFreyaMachineR d.Id

        match result with
        | true -> return d.True
        | _ -> return d.False }

let private handler (h: FreyaMachineHandlerNode) =
    freya {
        do! executionFreyaMachineR h.Id

        return h.Handler }

let private operation o =
    freya {
        do! o.Operation
        do! executionFreyaMachineR o.Id

        return o.Next }

let private traverse (graph: FreyaMachineGraph) =
    let rec eval from =
        freya {
            printfn "node: %s" from
            
            match Map.find from graph with
            | ActionNode a -> return! action a >>= eval
            | DecisionNode d -> return! decision d >>= eval
            | HandlerNode h -> return! handler h
            | OperationNode o -> return! operation o >>= eval }

    eval Decisions.ServiceAvailable

(* Compilation *)

let compileFreyaMachine (machine: FreyaMachine) : FreyaPipeline =
    let definition = snd (machine Map.empty)
    let graph = buildGraph definition
    let graphRecord = graphR graph

    freya {
        do! graphFreyaMachineR graphRecord
        do! setPLM definitionPLens definition
        do! traverse graph >>= represent

        return Halt }