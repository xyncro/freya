[<AutoOpen>]
module Freya.Inspector.Pipeline

open Freya.Core
open Freya.Core.Operators
open Freya.Pipeline
open Freya.Recorder
open Freya.Types
open Freya.Types.Http

(* Functions *)

let private init =
    (initR ()) *> next

let private display config =
    (render config <!> listR ()) *> halt

(* Pipeline *)

let freyaInspector config : FreyaPipeline =
    freya {
        let! path = getLM Request.path

        match path with
        | x when x = config.Path -> return! display config
        | _ -> return! init }