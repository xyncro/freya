module Freya.Integration.Tests.Core

open Freya.Core
open NUnit.Framework
open Swensen.Unquote

let private answerLens =
    environmentKeyLens "Answer"

(* Integration *)

open Freya.Integration
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