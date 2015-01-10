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
    let app =
        freya {
            do! setLM answerLens 42 }

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