[<AutoOpen>]
module Freya.Router.Recording

open Aether
open Freya.Core
open Freya.Recorder

(* Keys *)

let [<Literal>] private routerRecordKey =
    "router"

(* Types *)

type FreyaRouterRecord =
    { Execution: string list }

(* Contructors *)

let private freyaRouterRecord =
    { Execution = List.empty }

(* Lenses *)

let freyaRouterRecordPLens =
    recordDataPLens<FreyaRouterRecord> routerRecordKey

(* Functions *)

let internal initFreyaRouterR () =
    modR (setPL freyaRouterRecordPLens freyaRouterRecord)