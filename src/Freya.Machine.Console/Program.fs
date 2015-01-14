open System
open System.Collections.Generic
open Freya.Core
open Freya.Machine
open Freya.Machine.Http

[<EntryPoint>]
let main _ =

    let defaults =
        freyaMachine {
            using http }

    let test =
        freyaMachine {
            including defaults } |> Machine.toPipeline

    let env =
        { Environment = new Dictionary<string, obj> () :> IDictionary<string, obj>
          Meta = { Memos = Map.empty } }

    Console.ReadLine () |> ignore

    let _, state = test env |> Async.RunSynchronously

    Console.ReadLine () |> ignore
    0
