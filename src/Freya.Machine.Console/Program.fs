open Freya.Core
open Freya.Machine
open Freya.Machine.Operators

let c =
    { Configurable = false
      Configured = false }

let u = Core.returnM ()
let t = Core.returnM true

[<EntryPoint>]
let main _ =

    let http =
        { Name = "freya.machine.http"
          Dependencies = Set.empty
          Operations = 
            [ Start    /*> Finish
          
              Node "a" .|= DefinitionBinary (fun _ -> c, t)
              Node "b" .|= DefinitionUnary (fun _ -> c, u)
              Node "c" .|= DefinitionUnary (fun _ -> c, u)
          
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
        machine {
            using http
            using cors }
    
    System.Console.ReadLine () |> ignore

    0
