module Freya.Machine.Inspector

open Aether
open Fleece
open Freya.Inspector

(* Runtime *)

let private init =
    initFreyaMachineR ()

let private runtime =
    { Initialize = init }

(* Inspection *)

let private data =
    getPL freyaMachineRecordPLens >> Option.map toJSON

let private inspection =
    { FreyaInspectorInspection.Data = data }

(* Inspector *)

let freyaMachineInspector =
    { Id = machineKey
      Runtime = runtime
      Inspection = inspection }
