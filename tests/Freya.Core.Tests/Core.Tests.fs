namespace Freya.Core.Tests

open System.Collections.Generic
open Freya.Core
open Freya.Core.Operators
open NUnit.Framework
open Swensen.Unquote


[<AutoOpen>]
module Data =
    
    let env () =

        let data =
            dict ["value", box 0]

        Dictionary<string, obj> (data) :> IDictionary<string, obj>


[<AutoOpen>]
module Helpers =

    let test m =
        Async.RunSynchronously (m (env ()))


module Functions =

    [<Test>]
    let ``getM, setM behave correctly`` () =
        let m =
            freya {
                do! setM (Dictionary<string, obj> (dict ["value", box 1]))
                return! getM }

        let value, env = test m

        value.Count =? 1
        env.Count =? 1
        
        unbox value.["value"] =? 1
        unbox env.["value"] =? 1 

    [<Test>]
    let ``modM behaves correctly`` () =
        let m =
            freya {
                do! modM (fun x -> x.["value"] <- box 1; x)
                return! getM }

        let value, env = test m

        value.Count =? 1
        env.Count =? 1
        
        unbox value.["value"] =? 1
        unbox env.["value"] =? 1 


module Operators =

    [<Test>]
    let ``bind and map operators behave correctly`` () =
        let m1 = (fun (x: FreyaEnvironment) -> unbox x.["value"]) <!> getM
        let m2 = fun v -> modM (fun x -> x.["newvalue"] <- (v + 1); x)

        let _, env = test (m1 >>= m2)

        env.Count =? 2

        unbox env.["value"] =? 0
        unbox env.["newvalue"] =? 1
