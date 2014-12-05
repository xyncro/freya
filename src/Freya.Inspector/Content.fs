[<AutoOpen>]
module internal Freya.Inspector.Content

open Freya.Core
open Freya.Pipeline
open Freya.Router
open Freya.Types.Http

(* Resources *)

let page =
    freya {
        let message = System.Text.Encoding.UTF8.GetBytes "Freya Inspector"
        do! modLM Response.body (fun b -> b.Write (message, 0, message.Length); b)
        return Halt }

(* Routes *)

let GET =
    Methods [ GET ]

let content (config: FreyaInspectorConfiguration) =
    freyaRouter {
        route GET config.Path page } |> compileFreyaRouter