[<AutoOpen>]
module internal Freya.Inspector.Content

open Freya.Core.Operators
open Freya.Machine
open Freya.Router
open Freya.Types.Http

(* Content *)

let private cssContent =
    resource "site.css"

let private htmlContent = 
    resource "index.html"

(* Functions *)

let private getContent content n =
    represent n <!> returnM content

let private getCss =
    getContent cssContent

let private getHtml =
    getContent htmlContent

(* Resources *)

let private css =
    freyaMachine {
        including defaults
        mediaTypesSupported css
        handleOk getCss } |> compileFreyaMachine

let private html =
    freyaMachine {
        including defaults
        mediaTypesSupported html
        handleOk getHtml } |> compileFreyaMachine

(* Routes *)

let private GET =
    Methods [ GET ]

let content (config: FreyaInspectorConfiguration) =
    freyaRouter {
        route GET "/freya/inspect" html
        route GET "/freya/inspect/css" css } |> compileFreyaRouter