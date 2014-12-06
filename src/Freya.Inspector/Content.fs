[<AutoOpen>]
module internal Freya.Inspector.Content

open Freya.Core
open Freya.Core.Operators
open Freya.Machine
open Freya.Pipeline
open Freya.Router
open Freya.Types.Http

(* Functions *)

let private represent (n: FreyaMachineNegotiation) x =
    { Metadata =
        { Charset = Some n.Charsets.Head
          Encodings = None
          MediaType = Some n.MediaTypes.Head
          Languages = None }
      Data = x }

let private getPage n =
    represent n <!> returnM (resource "index.html")

(* Resources *)

let private defaults =
    freyaMachine {
        charsetsSupported utf8
        languagesSupported en
        methodsSupported get }

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
        route GET config.Path page } |> compileFreyaRouter