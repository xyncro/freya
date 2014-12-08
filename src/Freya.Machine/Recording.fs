[<AutoOpen>]
module Freya.Machine.Recording

open Aether
open Aether.Operators
open Fleece
open Fleece.Operators
open Freya.Core
open Freya.Recorder

(* Keys *)

let [<Literal>] internal machineKey =
    "machine"

(* Types *)

type FreyaMachineRecord =
    { Graph: FreyaMachineGraphRecord
      Execution: FreyaMachineExecutionRecord }

    static member ToJSON (x: FreyaMachineRecord) =
        jobj [
            "execution" .= x.Execution ]

(* Graph *)

and FreyaMachineGraphRecord =
    { Nodes: FreyaMachineGraphNodeRecord list
      Edges: FreyaMachineGraphEdgeRecord list }

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
    { Nodes: FreyaMachineExecutionNodeRecord list }

    static member ToJSON (x: FreyaMachineExecutionRecord) =
        jobj [
            "nodes" .= x.Nodes ]

and FreyaMachineExecutionNodeRecord =
    { Id: string }

    static member ToJSON (x: FreyaMachineExecutionNodeRecord) =
        jobj [
            "id" .= x.Id ]

(* Constructors *)

let private freyaMachineRecord =
    { Graph =
        { Nodes = List.empty
          Edges = List.empty }
      Execution =
        { Nodes = List.empty } }

(* Lenses *)

let private executionLens =
    (fun x -> x.Execution), (fun e x -> { x with Execution = e })

let private executionNodeLens =
    (fun x -> x.Nodes), (fun n x -> { x with FreyaMachineExecutionRecord.Nodes = n })

let internal freyaMachineRecordPLens =
    recordDataPLens<FreyaMachineRecord> machineKey

(* Functions *)

let internal initFreyaMachineR () =
    modR (setPL freyaMachineRecordPLens freyaMachineRecord)

let internal executeFreyaMachineR id =
    modR (modPL (freyaMachineRecordPLens >?-> executionLens >?-> executionNodeLens) 
                (fun es -> { Id = id } :: es))