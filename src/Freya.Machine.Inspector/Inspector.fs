module Freya.Machine.Inspector

open Aether
open Fleece
open Freya.Inspector

(* Runtime *)

let private initialize =
    initializeFreyaMachineRecord

let private runtime =
    { Initialize = initialize }

(* Inspection *)

let private data =
    getPL freyaMachineRecordPLens >> Option.map toJSON

let private inspection =
    { Data = data }

(* Inspector *)

let freyaMachineInspector =
    { Id = machineKey
      Runtime = runtime
      Inspection = inspection }
