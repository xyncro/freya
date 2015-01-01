module Freya.Router.Inspector

open Aether
open Fleece
open Freya.Inspector

(* Runtime *)

let private initialize =
    initializeFreyaRouterRecord

let private runtime =
    { Initialize = initialize }

(* Inspection *)

let private data =
    getPL freyaRouterRecordPLens >> Option.map toJSON

let private inspection =
    { Data = data }

(* Inspector *)

let freyaRouterInspector =
    { Id = routerKey
      Runtime = runtime
      Inspection = inspection }