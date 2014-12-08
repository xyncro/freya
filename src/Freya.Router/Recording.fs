[<AutoOpen>]
module Freya.Router.Recording

open Aether
open Fleece
open Fleece.Operators
open Freya.Core
open Freya.Recorder

(* Keys *)

let [<Literal>] internal routerKey =
    "router"

(* Types *)

type FreyaRouterRecord =
    { Execution: string list }

    static member ToJSON (x: FreyaRouterRecord) =
        jobj [
            "execution" .= x.Execution ]

(* Contructors *)

let private freyaRouterRecord =
    { Execution = List.empty }

(* Lenses *)

let internal freyaRouterRecordPLens =
    recordDataPLens<FreyaRouterRecord> routerKey

(* Functions *)

let internal initFreyaRouterR () =
    modR (setPL freyaRouterRecordPLens freyaRouterRecord)