module Freya.Integration.Tests.Core

open System.Threading.Tasks
open NUnit.Framework
open Swensen.Unquote
open Freya.Core
open Freya.Integration

(* AppFunc *)

let private answerLens =
    environmentKeyLens "Answer"

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

(** MidFunc **)

open Freya.Core.Operators
open Freya.Pipeline
open Freya.Pipeline.Operators

[<Test>]
let ``pipeline executes both monads if first returns next`` () =
    let app = setLM answerLens 42 |> OwinAppFunc.fromFreya
    let o1 = modM (fun x -> x.Environment.["o1"] <- true; x) *> next |> OwinMidFunc.fromFreya
    let o2 = modM (fun x -> x.Environment.["o2"] <- true; x) *> next |> OwinMidFunc.fromFreya
    let composed = o1.Invoke(o2.Invoke(app))

    let env = invoke composed

    unbox env.["o1"] =? true
    unbox env.["o2"] =? true
    env.ContainsKey("Answer") =? true
    unbox env.["Answer"] =? 42

[<Test>]
let ``pipeline executes only the first monad if first returns terminate`` () =
    let app = setLM answerLens 42 |> OwinAppFunc.fromFreya
    let o1 = modM (fun x -> x.Environment.["o1"] <- true; x) *> halt |> OwinMidFunc.fromFreya
    let o2 = modM (fun x -> x.Environment.["o2"] <- true; x) *> next |> OwinMidFunc.fromFreya
    let composed = o1.Invoke(o2.Invoke(app))

    let env = invoke composed

    unbox env.["o1"] =? true
    unbox env.["o2"] =? false
    env.ContainsKey("Answer") =? false

[<Test>]
let ``pipeline executes both monads if first returns next with composed pipeline`` () =
    let app = setLM answerLens 42 |> OwinAppFunc.fromFreya
    let o1 = modM (fun x -> x.Environment.["o1"] <- true; x) *> next
    let o2 = modM (fun x -> x.Environment.["o2"] <- true; x) *> next
    let pipe = o1 >?= o2 |> OwinMidFunc.fromFreya
    let composed = pipe.Invoke app

    let env = invoke composed

    unbox env.["o1"] =? true
    unbox env.["o2"] =? true
    env.ContainsKey("Answer") =? true
    unbox env.["Answer"] =? 42

[<Test>]
let ``pipeline executes only the first monad if first returns terminate with composed pipeline`` () =
    let app = setLM answerLens 42 |> OwinAppFunc.fromFreya
    let o1 = modM (fun x -> x.Environment.["o1"] <- true; x) *> halt
    let o2 = modM (fun x -> x.Environment.["o2"] <- true; x) *> next
    let pipe = o1 >?= o2 |> OwinMidFunc.fromFreya
    let composed = pipe.Invoke app

    let env = invoke composed

    unbox env.["o1"] =? true
    unbox env.["o2"] =? false
    env.ContainsKey("Answer") =? false
