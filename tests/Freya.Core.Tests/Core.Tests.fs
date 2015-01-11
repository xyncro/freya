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
            do! setLM answerLens 42
            let! v1 = getLM answerLens

            do! modLM answerLens ((*) 2)
            let! v2 = getLM answerLens

            return v1, v2 }

    let result = run m

    fst result =? (42, 84)

(* Integration *)

open Freya.Core.Integration
open System.Threading.Tasks

[<Test>]
let ``freya computation can compose with an OwinAppFunc`` () =
    let app =
        OwinAppFunc(fun (env: OwinEnvironment) ->
            env.["Answer"] <- 42
            Task.FromResult<obj>(null) :> Task)

    let converted = OwinAppFunc.toFreya app

    let m =
        freya {
            do! converted
            let! v1 = getLM answerLens
            return v1 }
    
    let result = run m
    fst result =? 42

[<Test>]
let ``freya computation can roundtrip to and from OwinAppFunc`` () =
    let app = setLM answerLens 42

    let converted =
        app
        |> OwinAppFunc.fromFreya
        |> OwinAppFunc.toFreya

    let m =
        freya {
            do! converted
            let! v1 = getLM answerLens
            return v1 }
    
    let result = run m
    fst result =? 42

(* Operators *)

open Freya.Core.Operators

[<Test>]
let ``apply correctly unwraps and applies Freya computations`` () =
    let comp = Freya.returnM 2
    let f = Freya.returnM ((+) 40)
    let test = Freya.apply f comp
    let result = run test
    fst result =? 42

[<Test>]
let ``map correctly applies a function to a Freya computation`` () =
    let test =
        Freya.returnM 40
        |> Freya.map ((+) 2)
    let result = run test
    fst result =? 42

[<Test>]
let ``map2 correctly applies a function to Freya computations`` () =
    let comp1 = Freya.returnM 2
    let comp2 = Freya.returnM 40
    let test = Freya.map2 (+) comp1 comp2
    let result = run test
    fst result =? 42

[<Test>]
let ``map + apply correctly chain across applicative computations`` () =
    let comp1 = Freya.returnM 2
    let comp2 = Freya.returnM 40
    let test = (+) <!> comp1 <*> comp2
    let result = run test
    fst result =? 42
