namespace Dyfrig.Pipeline

open Dyfrig.Core


type Pipeline =
    OwinMonad<PipelineChoice>

and PipelineChoice =
    | Next
    | Terminate


[<AutoOpen>]
module Return =

    let next _ : Pipeline =
        owin { return Next }

    let terminate _ : Pipeline =
        owin { return Terminate }


[<AutoOpen>]
module Composition =

    let pipeline (p1: Pipeline) (p2: Pipeline) : Pipeline =
        owin {
            let! pc = p1

            match pc with
            | Next -> return! p2
            | _ -> return Terminate }


module Operators =

    let (>?=) p1 p2 : Pipeline = 
        pipeline p1 p2
