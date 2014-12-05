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

and FreyaMachineExecutionRecord =
    { Id: string }

(* Constructors *)

let private machineRecord =
    { Execution = List.empty }

(* Lenses *)

let executionLens =
    (fun x -> x.Execution), (fun e x -> { x with Execution = e })

(* Functions *)

let initFreyaMachineR () =
    setR "freya.Machine" machineRecord

let executeFreyaMachineR id =
    modR "freya.Machine" (modL executionLens (fun es -> { Id = id } :: es))
