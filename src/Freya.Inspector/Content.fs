[<AutoOpen>]
module internal Freya.Inspector.Content

open Freya.Core.Operators
open Freya.Machine
open Freya.Router
open Freya.Types.Http

(* Content *)

let private cssContent =
    resource "site.css"

let private pageContent =
    resource "index.html"

(* Functions *)

let private getCss n =
    represent n <!> returnM cssContent

let private getPage n =
    represent n <!> returnM pageContent

(* Resources *)

let private css =
    freyaMachine {
        including defaults
        mediaTypesSupported css
        handleOk getCss } |> compileFreyaMachine

let private page =
    freyaMachine {
        including defaults
        mediaTypesSupported html
        handleOk getPage } |> compileFreyaMachine

(* Routes *)

let private GET =
    Methods [ GET ]

let content (config: FreyaInspectorConfiguration) =
    freyaRouter {
        route GET "/freya/inspect" page
        route GET "/freya/inspect/css" css } |> compileFreyaRouter