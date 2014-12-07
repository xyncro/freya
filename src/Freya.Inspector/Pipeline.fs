[<AutoOpen>]
module Freya.Inspector.Pipeline

open Freya.Core
open Freya.Core.Operators
open Freya.Pipeline
open Freya.Pipeline.Operators
open Freya.Recorder
open Freya.Types.Http

(* Functions *)

let private initialize =
    freya {
        return! initR () *> next }

let private record =
    freya {
        let! meth = getLM Request.meth
        let! path = getLM Request.path

        return! initFreyaR meth path *> next }

(* Pipelines *)

let private inspect config =
        content config 
    >?= data config

let freyaInspector config =
        inspect config 
    >?= initialize 
    >?= record