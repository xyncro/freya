[<AutoOpen>]
module Freya.Machine.Execution

open Freya.Core
open Freya.Core.Operators
open Freya.Pipeline
open Freya.Typed

(* Traversal *)

let private action a =
    freya {
        do! a.Action
        do! executionI (ActionLog {
            Name = a.Id
            Overridden = a.Override.Overridden })

        return a.Next }

let private decision d =
    freya {
        let! next, result = 
               (function | true -> d.True, true 
                         | _ -> d.False, false) 
            <!> d.Decision

        do! executionI (DecisionLog { 
                    Name = d.Id
                    Overridden = d.Override.Overridden
                    Result = result
                    Next = next })

        return next }

let private handler (h: FreyaMachineHandlerNode) =
    freya {
        do! executionI (HandlerLog {
            Name = h.Id
            Overridden = h.Override.Overridden })

        return h.Handler }

let private operation o =
    freya {
        do! o.Operation
        do! executionI (OperationLog {
            Name = o.Id })

        return o.Next }

let private traverse (graph: FreyaMachineGraph) =
    let rec eval from =
        freya {
            match Map.find from graph with
            | ActionNode a -> return! action a >>= eval
            | DecisionNode d -> return! decision d >>= eval
            | HandlerNode h -> return! handler h
            | OperationNode o -> return! operation o >>= eval  }

    eval Decisions.ServiceAvailable

(* Compilation *)

let private nodes =
      actions
    @ decisions
    @ handlers
    @ operations
    
let compileFreyaMachine (m: FreyaMachine) : FreyaPipeline =
    let _, definition = m Map.empty
    let graph = construct definition nodes

    freya {
        do! initI
        do! setPLM definitionPLens definition
        do! traverse graph >>= represent

        return Halt }
