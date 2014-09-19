namespace Dyfrig.Pipeline

open Dyfrig.Core


type Pipeline =
    OwinMonad<PipelineChoice>

and PipelineChoice =
    | Proceed
    | Halt


[<AutoOpen>]
module Return =

    let proceed _ : Pipeline =
        owin { return Proceed }

    let halt _ : Pipeline =
        owin { return Halt }


[<AutoOpen>]
module Composition =

    let pipeline (p1: Pipeline) (p2: Pipeline) : Pipeline =
        owin {
            let! pc = p1

            match pc with
            | Proceed -> return! p2
            | _ -> return Halt }


module Operators =

    let (>?=) p1 p2 : Pipeline = 
        pipeline p1 p2
