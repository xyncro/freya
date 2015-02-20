module Freya.Core.Tests.Core

open Freya.Core
open NUnit.Framework
open Swensen.Unquote

let private answerLens =
    environmentKeyLens "Answer"

[<Test>]
let ``getLM, setLM, modLM behave correctly`` () =
    let m =
        freya {
            do! Freya.setLens answerLens 42
            let! v1 = Freya.getLens answerLens

            do! Freya.mapLens answerLens ((*) 2)
            let! v2 = Freya.getLens answerLens

            return v1, v2 }

    let result = run m

    fst result =? (42, 84)

(* Integration *)

open System
open System.Collections.Generic
open System.Threading.Tasks

[<Test>]
let ``freya computation can be run as an OWIN application`` () =
    let m = setLM answerLens 42

    let app = OwinAppFunc.ofFreya m
    let env = Dictionary<string, obj>() :> IDictionary<string, obj>
    let converted = OwinAppFunc.toFreya app

    let m =
        freya {
            do! converted
            let! v1 = Freya.getLens answerLens
            return v1 }
    
    let result = run m
    fst result =? 42

[<Test>]
let ``freya computation can roundtrip to and from OwinAppFunc`` () =
    let app = Freya.setLens answerLens 42

    let converted =
        app
        |> OwinAppFunc.fromFreya
        |> OwinAppFunc.toFreya

    let m =
        freya {
            do! converted
            let! v1 = Freya.getLens answerLens
            return v1 }
    
    app.Invoke(env).ContinueWith<unit>(fun _ -> ())
    |> Async.AwaitTask
    |> Async.RunSynchronously
    env.["Answer"] |> unbox =? 42

(* Operators *)

open Freya.Core.Operators

[<Test>]
let ``apply correctly unwraps and applies Freya computations`` () =
    let comp = Freya.init 2
    let f = Freya.init ((+) 40)
    let test = Freya.apply f comp
    let result = run test
    fst result =? 42

[<Test>]
let ``map correctly applies a function to a Freya computation`` () =
    let test =
        Freya.init 40
        |> Freya.map ((+) 2)
    let result = run test
    fst result =? 42

[<Test>]
let ``map2 correctly applies a function to Freya computations`` () =
    let comp1 = Freya.init 2
    let comp2 = Freya.init 40
    let test = Freya.map2 (+) comp1 comp2
    let result = run test
    fst result =? 42

[<Test>]
let ``map + apply correctly chain across applicative computations`` () =
    let comp1 = Freya.init 2
    let comp2 = Freya.init 40
    let test = (+) <!> comp1 <*> comp2
    let result = run test
    fst result =? 42
