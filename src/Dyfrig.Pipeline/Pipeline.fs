module Dyfrig.Pipeline

open Dyfrig.Core
open Dyfrig.Core.Operators

(* Types *)

type Pipeline =
    OwinMonad<PipelineChoice>

and PipelineChoice =
    | Next
    | Halt

(* Helpers *)

let next : Pipeline =
    returnM Next

let halt : Pipeline =
    returnM Halt

(* Composition *)

let compose (p1: Pipeline) (p2: Pipeline) : Pipeline =
    owin {
        let! pc = p1

        match pc with
        | Next -> return! p2
        | _ -> return Halt }


module Operators =

    let (>?=) p1 p2 : Pipeline = 
        compose p1 p2
