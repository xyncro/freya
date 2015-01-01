[<AutoOpen>]
module Freya.Inspector.Inspector

open Aether
open Fleece
open Freya.Core
open Freya.Types.Http

(* Runtime *)

let private initialize =
    freya {
        let! meth = getLM Request.meth
        let! path = getLM Request.path

        do! initializeFreyaRequestRecord meth path }

let private runtime =
    { Initialize = initialize }

(* Inspection *)

let private data =
    getPL freyaRequestRecordPLens >> Option.map toJSON

let private inspection =
    { Data = data }

(* Inspector *)

let freyaRequestInspector =
    { Id = requestKey
      Runtime = runtime
      Inspection = inspection }