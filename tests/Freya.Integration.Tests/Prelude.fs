[<AutoOpen>]
module internal Freya.Integration.Tests.Prelude

open System.Collections.Generic
open Freya.Core

let private freyaState () =
    let env = 
        Dictionary<string, obj> () :> IDictionary<string, obj>

    { Environment = env
      Meta =
        { Memos = Map.empty } }

let run m =
    Async.RunSynchronously (m (freyaState ()))