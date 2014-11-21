[<AutoOpen>]
module Freya.Machine.Execution

open Freya.Core
open Freya.Core.Operators
open Freya.Pipeline
open Freya.Typed

//[<AutoOpen>]
//module Cache =
//    
//    let cache<'T> m =
//        let lens =
//                 dictPLens (string (Guid.NewGuid ()))
//            <?-> boxIso<'T>
//
//        owin {
//            let! value = getPLM lens
//
//            match value with
//            | Some cached ->
//                return cached
//            | _ ->
//                let! created = m
//                do! setPLM lens created
//
//                return created }


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
        let! result = d.Decision
        let next = (function | true -> d.True | _ -> d.False) result

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
