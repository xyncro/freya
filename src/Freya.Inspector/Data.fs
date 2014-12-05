[<AutoOpen>]
module internal Freya.Inspector.Data

open Freya.Core
open Freya.Pipeline
open Freya.Router

(* Resources *)

let xs =
    freya {
        return Halt }

(* Routes *)

let data (config: FreyaInspectorConfiguration) =
    freyaRouter {
        route All config.Path xs } |> compileFreyaRouter