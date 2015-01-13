open System
open System.Collections.Generic
open Freya.Core
open Freya.Machine
open Freya.Machine.Http

let defaults =
    freyaMachine {
        using http }

let test =
    freyaMachine {
        including defaults
        serviceAvailable (Freya.init false) } |> reifyMachine

let env =
    { Environment = new Dictionary<string, obj> () :> IDictionary<string, obj>
      Meta = { Memos = Map.empty } }

[<EntryPoint>]
let main _ =

    let _, state = test env |> Async.RunSynchronously

    Console.ReadLine () |> ignore
    0
