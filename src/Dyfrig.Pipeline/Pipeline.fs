namespace Dyfrig.Pipeline

open Dyfrig.Core


type Pipeline =
    OwinMonad<PipelineChoice>

and PipelineChoice =
    | Next
    | Halt


[<AutoOpen>]
module Return =

    let next : Pipeline =
        owin { return Next }

    let halt : Pipeline =
        owin { return Halt }


[<AutoOpen>]
module Composition =

    let pipeline (p1: Pipeline) (p2: Pipeline) : Pipeline =
        owin {
            let! pc = p1

            match pc with
            | Next -> return! p2
            | _ -> return Halt }


module Operators =

    let (>?=) p1 p2 : Pipeline = 
        pipeline p1 p2
