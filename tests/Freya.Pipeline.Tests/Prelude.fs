[<AutoOpen>]
module internal Freya.Pipeline.Tests.Prelude

open System.Collections.Generic
open Freya.Core

let private freyaState () =
    let env = 
        Dictionary<string, obj> () :> IDictionary<string, obj>

    env.["o1"] <- false
    env.["o2"] <- false

    { Environment = env
      Meta =
        { Memos = Map.empty } }

let run m =
    Async.RunSynchronously (m (freyaState ()))