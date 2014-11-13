module Freya.Pipeline

open Freya.Core
open Freya.Core.Operators

(* Types *)

type FreyaPipeline =
    Freya<FreyaPipelineChoice>

and FreyaPipelineChoice =
    | Next
    | Halt

(* Helpers *)

let next : FreyaPipeline =
    returnM Next

let halt : FreyaPipeline =
    returnM Halt

(* Composition *)

let compose (p1: FreyaPipeline) (p2: FreyaPipeline) : FreyaPipeline =
    freya {
        let! pc = p1

        match pc with
        | Next -> return! p2
        | _ -> return Halt }


module Operators =

    let (>?=) p1 p2 : FreyaPipeline = 
        compose p1 p2
