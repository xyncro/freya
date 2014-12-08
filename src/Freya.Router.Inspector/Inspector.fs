module Freya.Router.Inspector

open Aether
open Fleece
open Freya.Inspector

(* Runtime *)

let private init =
    initFreyaRouterR ()

let private runtime =
    { Initialize = init }

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