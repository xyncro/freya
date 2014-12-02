[<AutoOpen>]
module Freya.Machine.Inspection

open Aether
open Aether.Operators
open Freya.Core
open Freya.Inspector.Core

(* Literals *)

let [<Literal>] freyaMachineInspectionKey = 
    "Freya.Machine"

(* Types *)

type FreyaMachineInspection =
    { Execution: FreyaMachineExecutionLog list }

and FreyaMachineExecutionLog =
    | ActionLog of FreyaMachineActionLog
    | DecisionLog of FreyaMachineDecisionLog
    | HandlerLog of FreyaMachineHandlerLog
    | OperationLog of FreyaMachineOperationLog

and FreyaMachineActionLog =
    { Name: string
      Overridden: bool }

and FreyaMachineDecisionLog =
    { Name: string
      Overridden: bool
      Result: bool
      Next: string }

and FreyaMachineHandlerLog =
    { Name: string
      Overridden: bool }

and FreyaMachineOperationLog =
    { Name: string }

(* Constructors *)

let private machineInspection =
    { Execution = List.empty }

(* Lenses *)

let internal executionLens =
    (fun x -> x.Execution), (fun e x -> { x with Execution = e })

(* Functions *)

let internal initI : Freya<unit> =
    setI freyaMachineInspectionKey machineInspection

let internal executionI e =
    modI freyaMachineInspectionKey (modL executionLens (fun es -> e :: es))
