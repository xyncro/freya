[<AutoOpen>]
module Freya.Machine.Recording

open Aether
open Aether.Operators
open Freya.Core
open Freya.Recorder

(* Keys *)

let [<Literal>] private machineRecordKey =
    "machine"

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

let private freyaMachineRecord =
    { Execution = List.empty }

(* Lenses *)

let private executionLens =
    (fun x -> x.Execution), (fun e x -> { x with Execution = e })

let freyaMachineRecordPLens =
    recordDataPLens<FreyaMachineRecord> machineRecordKey

(* Functions *)

let internal initFreyaMachineR () =
    modR (setPL freyaMachineRecordPLens freyaMachineRecord)

let internal executeFreyaMachineR id =
    modR (modPL (freyaMachineRecordPLens >?-> executionLens) (fun es -> { Id = id } :: es))