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

let private record config =
    freya {
        for inspector in (config.Inspectors) do
            do! inspector.Runtime.Initialize

        return! next }

(* Pipelines *)

let private inspect config =
        content config 
    >?= data config

let freyaInspector config =
        inspect config 
    >?= initialize 
    >?= record config