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
            "graph" .= x.Graph
            "execution" .= x.Execution ]

(* Graph *)

and FreyaMachineGraphRecord =
    { Nodes: FreyaMachineGraphNodeRecord list
      Edges: FreyaMachineGraphEdgeRecord list }

    static member ToJSON (x: FreyaMachineGraphRecord) =
        jobj [
            "nodes" .= x.Nodes
            "edges" .= x.Edges ]

and FreyaMachineGraphNodeRecord =
    { Id: string
      Type: string
      AllowOverride: bool
      HasOverride: bool }

    static member ToJSON (x: FreyaMachineGraphNodeRecord) =
        jobj [
            "id" .= x.Id
            "type" .= x.Type
            "allowOverride" .= x.AllowOverride
            "hasOverride" .= x.HasOverride ]

and FreyaMachineGraphEdgeRecord =
    { From: string
      To: string }

    static member ToJSON (x:FreyaMachineGraphEdgeRecord) =
        jobj [
            "from" .= x.From
            "to" .= x.To ]

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

let private freyaMachineGraphNodeRecord id t allow has =
    { Id = id
      Type = t
      AllowOverride = allow
      HasOverride = has }

let private freyaMachineGraphEdgeRecord from t =
    { From = from
      To = t }

(* Lenses *)

let private graphLens =
    (fun x -> x.Graph), (fun g x -> { x with Graph = g })

let private executionLens =
    (fun x -> x.Execution), (fun e x -> { x with Execution = e })

let private executionNodeLens =
    (fun x -> x.Nodes), (fun n x -> { x with FreyaMachineExecutionRecord.Nodes = n })

let internal freyaMachineRecordPLens =
    recordDataPLens<FreyaMachineRecord> machineKey

(* Functions *)

let private node id x =
    let node = freyaMachineGraphNodeRecord id

    match x with
    | ActionNode { Override = o } -> 
        node "action" o.Allow o.Overridden
    | DecisionNode { Override = o } -> 
        node "decision" o.Allow o.Overridden
    | HandlerNode { Override = o } -> 
        node "handler" o.Allow o.Overridden
    | OperationNode _ -> 
        node "operation" false false

let private edges id x =
    let edge = freyaMachineGraphEdgeRecord id

    match x with
    | ActionNode x ->
        [ edge x.Next ]
    | DecisionNode x ->
        [ edge x.True
          edge x.False ]
    | HandlerNode _ ->
        []
    | OperationNode x ->
        [ edge x.Next ]

let internal graphR g =
    let list = Map.toList g
    let nodes = List.map (fun (k, v) -> node k v)
    let edges = List.map (fun (k, v) -> edges k v) >> List.concat

    { Nodes = nodes list
      Edges = edges list }

(* Recording *)

let internal initFreyaMachineR () =
    modR (setPL freyaMachineRecordPLens freyaMachineRecord)

let internal graphFreyaMachineR graph =
    modR (setPL (freyaMachineRecordPLens >?-> graphLens) graph)

let internal executionFreyaMachineR id =
    modR (modPL (freyaMachineRecordPLens >?-> executionLens >?-> executionNodeLens) 
                (fun es -> es @ [ { Id = id } ]))