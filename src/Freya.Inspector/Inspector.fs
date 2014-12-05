[<AutoOpen>]
module internal Freya.Inspector.Inspector

open Freya.Core
open Freya.Pipeline
open Freya.Pipeline.Operators
open Freya.Router
open Freya.Types.Http

(* Resources *)

let page =
    freya {
        let message = System.Text.Encoding.UTF8.GetBytes "Freya Inspector"
        do! modLM Response.body (fun b -> b.Write (message, 0, message.Length); b)
        return Halt }

let data =
    freya {
        return Halt }

(* Routes *)

let GET =
    Methods [ GET ]

let ui (config: FreyaInspectorConfiguration) =
    freyaRouter {
        route GET config.Path page } |> compileFreyaRouter

let api (config: FreyaInspectorConfiguration) =
    freyaRouter {
        route All config.Path data } |> compileFreyaRouter

(* Pipeline *)

let inspector (config: FreyaInspectorConfiguration) =
    ui config >?= api config