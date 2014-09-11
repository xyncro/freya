namespace Dyfrig.Pipeline

open Dyfrig.Core


type Pipeline =
    OwinMonad<PipelineChoice>

and PipelineChoice =
    | Continue
    | Terminate


[<AutoOpen>]
module Composition =

    let pipelineChain o1 o2 =
        owin {
            do! o1
            return! o2 }

    let pipelinePipe o1 o2 =
        owin {
            let! pipelineChoice = o1

            match pipelineChoice with
            | Continue -> return! o2
            | Terminate -> return Terminate }


module Operators =

    let inline (-->) o1 o2 = pipelineChain o1 o2

    let inline (-?>) o1 o2 = pipelinePipe o1 o2
