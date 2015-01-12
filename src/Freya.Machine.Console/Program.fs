open Freya.Core
open Freya.Machine
open Freya.Machine.Operators

let c =
    { Configurable = false
      Configured = false }

let u = Freya.init ()
let t = Freya.init true

[<EntryPoint>]
let main _ =

    let http =
        { Name = "freya.machine.http"
          Dependencies = Set.empty
          Operations = 
            [ Start     /*>  Finish
          
              Ref "a"   .|=  Binary (fun _ -> c, t)
              Ref "b"   .|=  Unary (fun _ -> c, u)
              Ref "c"   .|=  Unary (fun _ -> c, u)
          
              Start     ..>  Ref "a"
              Ref "a"   .+>  Ref "b"
              Ref "a"   .->  Ref "c"
              Ref "b"   ..>  Finish
              Ref "c"   ..>  Finish ] }

    let cors =
        { Name = "freya.machine.cors"
          Dependencies = set [ "freya.machine.http" ]
          Operations = List.empty }

    let resource =
        freyaMachine {
            using http
            using cors }
    
    System.Console.ReadLine () |> ignore

    0
