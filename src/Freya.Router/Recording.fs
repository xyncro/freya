[<AutoOpen>]
module Freya.Router.Recording

//open Aether
//open Aether.Operators
open Freya.Core
open Freya.Recorder

(* Types *)

type FreyaRouterRecord =
    { Execution: string list }

(* Contructors *)

let private routerRecord =
    { Execution = List.empty }

(* Functions *)

let internal initR : Freya<unit> =
    setR "freya.Router" routerRecord