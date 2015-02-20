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
            let! v1 = Freya.getLens answerLens
            return v1 }
    
    let result = run m
    fst result =? 42

[<Test>]
let ``freya computation can roundtrip to and from OwinAppFunc`` () =
    let app = Freya.setLens answerLens 42

    let converted =
        app
        |> OwinAppFunc.ofFreya
        |> OwinAppFunc.toFreya

    let m =
        freya {
            do! converted
            let! v1 = Freya.getLens answerLens
            return v1 }
    
    let result = run m
    fst result =? 42

(** MidFunc **)

open Freya.Core.Operators
open Freya.Pipeline
open Freya.Pipeline.Operators

[<Test>]
let ``pipeline executes both monads if first returns next`` () =
    let app = Freya.setLens answerLens 42 |> OwinAppFunc.ofFreya
    let o1 = Freya.mapState (fun x -> x.Environment.["o1"] <- true; x) *> next |> OwinMidFunc.ofFreya
    let o2 = Freya.mapState (fun x -> x.Environment.["o2"] <- true; x) *> next |> OwinMidFunc.ofFreya
    let composed = o1.Invoke(o2.Invoke(app))

    let env = invoke composed

    unbox env.["o1"] =? true
    unbox env.["o2"] =? true
    env.ContainsKey("Answer") =? true
    unbox env.["Answer"] =? 42

[<Test>]
let ``pipeline executes only the first monad if first returns terminate`` () =
    let app = Freya.setLens answerLens 42 |> OwinAppFunc.ofFreya
    let o1 = Freya.mapState (fun x -> x.Environment.["o1"] <- true; x) *> halt |> OwinMidFunc.ofFreya
    let o2 = Freya.mapState (fun x -> x.Environment.["o2"] <- true; x) *> next |> OwinMidFunc.ofFreya
    let composed = o1.Invoke(o2.Invoke(app))

    let env = invoke composed

    unbox env.["o1"] =? true
    unbox env.["o2"] =? false
    env.ContainsKey("Answer") =? false

[<Test>]
let ``pipeline executes both monads if first returns next with composed pipeline`` () =
    let app = Freya.setLens answerLens 42 |> OwinAppFunc.ofFreya
    let o1 = Freya.mapState (fun x -> x.Environment.["o1"] <- true; x) *> next
    let o2 = Freya.mapState (fun x -> x.Environment.["o2"] <- true; x) *> next
    let pipe = o1 >?= o2 |> OwinMidFunc.ofFreya
    let composed = pipe.Invoke app

    let env = invoke composed

    unbox env.["o1"] =? true
    unbox env.["o2"] =? true
    env.ContainsKey("Answer") =? true
    unbox env.["Answer"] =? 42

[<Test>]
let ``pipeline executes only the first monad if first returns terminate with composed pipeline`` () =
    let app = Freya.setLens answerLens 42 |> OwinAppFunc.ofFreya
    let o1 = Freya.mapState (fun x -> x.Environment.["o1"] <- true; x) *> halt
    let o2 = Freya.mapState (fun x -> x.Environment.["o2"] <- true; x) *> next
    let pipe = o1 >?= o2 |> OwinMidFunc.ofFreya
    let composed = pipe.Invoke app

    let env = invoke composed

    unbox env.["o1"] =? true
    unbox env.["o2"] =? false
    env.ContainsKey("Answer") =? false

[<Test>]
let ``pipeline executes both monads if first returns next with wrapped pipeline OwinMidFunc`` () =
    let app = Freya.setLens answerLens 42 |> OwinAppFunc.ofFreya
    let o1 = Freya.mapState (fun x -> x.Environment.["o1"] <- true; x) *> next
    let o2 = Freya.mapState (fun x -> x.Environment.["o2"] <- true; x) *> next
    let midFunc = OwinMidFunc.ofFreyaWrapped o1 o2
    let composed = midFunc.Invoke app

    let env = invoke composed

    // TODO: show that o2 was run after app
    unbox env.["o1"] =? true
    unbox env.["o2"] =? true
    env.ContainsKey("Answer") =? true
    unbox env.["Answer"] =? 42

[<Test>]
let ``pipeline executes only the first monad if first returns terminate with wrapped pipeline OwinMidFunc`` () =
    let app = Freya.setLens answerLens 42 |> OwinAppFunc.ofFreya
    let o1 = Freya.mapState (fun x -> x.Environment.["o1"] <- true; x) *> halt
    let o2 = Freya.mapState (fun x -> x.Environment.["o2"] <- true; x) *> next
    let midFunc = OwinMidFunc.ofFreyaWrapped o1 o2
    let composed = midFunc.Invoke app

    let env = invoke composed

    // TODO: show that o2 was run after app
    unbox env.["o1"] =? true
    unbox env.["o2"] =? false
    env.ContainsKey("Answer") =? false

[<Test; Explicit("Implementation has a bug that causes the test to hang.")>]
let ``MidFunc can be split and used to wrap a pipeline`` () =
    let stopwatch = System.Diagnostics.Stopwatch()
    let o1 =
        Freya.mapState (fun x ->
            x.Environment.["o1"] <- true
            x.Environment.["o1 time"] <- stopwatch.ElapsedMilliseconds
            x)
        *> next
    let o2 =
        Freya.mapState (fun x ->
            x.Environment.["o2"] <- true
            x.Environment.["o2 time"] <- stopwatch.ElapsedMilliseconds
            x)
        *> next
    let midFunc =
        OwinMidFunc(fun next ->
            OwinAppFunc(fun env ->
                async {
                    env.["o3"] <- true
                    env.["o3 time"] <- stopwatch.ElapsedMilliseconds
                    do! next.Invoke(env).ContinueWith<unit>(fun _ -> ()) |> Async.AwaitTask
                    env.["o4"] <- true
                    env.["o4 time"] <- stopwatch.ElapsedMilliseconds
                } |> Async.StartAsTask :> Task ))
    let before, after = OwinMidFunc.splitIntoFreya midFunc

    stopwatch.Start()
    let result = run (before >?= o1 >?= o2 >?= after)
    stopwatch.Stop()

    let env = (snd result).Environment
    unbox env.["o1"] =? true
    unbox env.["o2"] =? true
    env.ContainsKey("o3") =? true
    unbox env.["o3"] =? true
    env.ContainsKey("o4") =? true
    unbox env.["o4"] =? true
    unbox env.["o3 time"] <? unbox env.["o1 time"]
    unbox env.["o1 time"] <? unbox env.["o2 time"]
    unbox env.["o2 time"] <? unbox env.["o4 time"]
