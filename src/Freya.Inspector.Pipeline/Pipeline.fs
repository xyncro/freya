[<AutoOpen>]
module Freya.Inspector.Pipeline.Pipeline

open Freya.Core
open Freya.Core.Operators
open Freya.Inspector.Core
open Freya.Pipeline
open Freya.Typed

let private init =
    (initE ()) *> next

let private display config =
    (render config <!> listE ()) *> halt

(* Pipeline *)

let freyaInspector config : FreyaPipeline =
    freya {
        let! path = getLM Request.path

        match path with
        | x when x = config.Path -> return! display config
        | _ -> return! init }