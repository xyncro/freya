open Freya.Core
open Freya.Core.Operators
open Freya.Machine
open Freya.Machine.Operators

let u = returnM ()
let t = returnM true

[<EntryPoint>]
let main _ =

    let http =
        { Name = "freya.machine.http"
          Dependencies = Set.empty
          Operations = 
            [ Start    /*> Finish
          
              Node "a" .|= Binary t
              Node "b" .|= Unary u
              Node "c" .|= Unary u
          
              Start    ..> Node "a"
              Node "a" .+> Node "b"
              Node "a" .-> Node "c"
              Node "b" ..> Finish
              Node "c" ..> Finish ] }

    let cors =
        { Name = "freya.machine.cors"
          Dependencies = set [ "freya.machine.http" ]
          Operations = List.empty }

    let resource =
        freyaMachine {
            using http
            using cors }





    let graph = 
        applyExtensions (set [ http; cors ]) (machineDefinitionGraph ())
    
    System.Console.ReadLine () |> ignore

    0
