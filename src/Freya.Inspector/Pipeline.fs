[<AutoOpen>]
module Freya.Inspector.Pipeline

open Freya.Core
open Freya.Core.Operators
open Freya.Pipeline
open Freya.Pipeline.Operators
open Freya.Recorder
open Freya.Router
open Freya.Types.Http

(* Initialization *)

let private init =
    freya {
        let! meth = getLM Request.meth
        let! path = getLM Request.path

        do! initR ()
        do! initFreyaR meth path

        return Next }

(* Pipeline *)

let freyaInspector config =
    inspector config >?= init