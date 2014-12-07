[<AutoOpen>]
module Freya.Machine.Recording

open Aether
open Freya.Core
open Freya.Recorder

(* Types *)

type FreyaMachineRecord =
    { Execution: FreyaMachineExecutionRecord list }

and FreyaMachineExecutionRecord =
    | ActionRecord of FreyaMachineActionRecord
    | DecisionRecord of FreyaMachineDecisionRecord
    | HandlerRecord of FreyaMachineHandlerRecord
    | OperationRecord of FreyaMachineOperationRecord

and FreyaMachineActionRecord =
    { Name: string
      Overridden: bool }

and FreyaMachineDecisionRecord =
    { Name: string
      Overridden: bool
      Result: bool
      Next: string }

and FreyaMachineHandlerRecord =
    { Name: string
      Overridden: bool }

and FreyaMachineOperationRecord =
    { Name: string }

(* Constructors *)

let private machineRecord =
    { Execution = List.empty }

(* Lenses *)

let internal executionLens =
    (fun x -> x.Execution), (fun e x -> { x with Execution = e })

(* Functions *)

let internal initR : Freya<unit> =
    setR "freya.Machine" machineRecord

let internal executionR e =
    modR "freya.Machine" (modL executionLens (fun es -> e :: es))
