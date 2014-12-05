[<AutoOpen>]
module internal Freya.Machine.Recording

open Aether
open Aether.Operators
open Freya.Core
open Freya.Recorder

(* Types *)

type FreyaMachineRecord =
    { Execution: FreyaMachineExecutionRecord list }

(* Graph *)

and FreyaMachineGraphRecord =
    { Nodes: FreyaMachineGraphNodeRecord
      Edges: FreyaMachineGraphEdgeRecord }

and FreyaMachineGraphNodeRecord =
    { Id: string
      Type: string
      AllowOverride: bool
      HasOverride: bool }

and FreyaMachineGraphEdgeRecord =
    { From: string
      To: string }

(* Execution *)

//and FreyaMachineExecutionRecord =
//    | ActionRecord of FreyaMachineActionRecord
//    | DecisionRecord of FreyaMachineDecisionRecord
//    | HandlerRecord of FreyaMachineHandlerRecord
//    | OperationRecord of FreyaMachineOperationRecord

and FreyaMachineExecutionRecord =
    { Id: string }

//and FreyaMachineDecisionRecord =
//    { Id: string
//      Result: bool
//      Next: string }
//
//and FreyaMachineHandlerRecord =
//    { Id: string }
//
//and FreyaMachineOperationRecord =
//    { Id: string }

(* Constructors *)

let private machineRecord =
    { Execution = List.empty }

(* Lenses *)

let executionLens =
    (fun x -> x.Execution), (fun e x -> { x with Execution = e })

(* Functions *)

let initR () =
    setR "freya.Machine" machineRecord

let executionR e =
    modR "freya.Machine" (modL executionLens (fun es -> e :: es))
