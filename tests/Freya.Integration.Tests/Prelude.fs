[<AutoOpen>]
module internal Freya.Integration.Tests.Prelude

open System.Collections.Generic
open Freya.Core

let env () =
    let e = Dictionary<string, obj> () :> IDictionary<string, obj>
    e.["o1"] <- false
    e.["o2"] <- false
    e

let private freyaState () =
    { Environment = env()
      Meta =
        { Memos = Map.empty } }

let invoke (composed: OwinAppFunc) =
    let e = env()
    composed.Invoke(e).ContinueWith<unit>(fun _ -> ())
    |> Async.AwaitTask
    |> Async.RunSynchronously
    e

let run m =
    Async.RunSynchronously (m (freyaState ()))