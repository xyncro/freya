module Freya.Integration.Tests.Core

open System.Threading.Tasks
open NUnit.Framework
open Swensen.Unquote
open Freya.Core
open Freya.Integration

(* AppFunc *)

let private answerLens =
    environmentKeyPLens "Answer"

let private o1Lens =
    environmentKeyLens "o1"

let private o2Lens =
    environmentKeyLens "o2"

[<Test>]
let ``freya computation can compose with an OwinAppFunc`` () =
    let app =
        OwinAppFunc(fun (env: OwinEnvironment) ->
            env.["Answer"] <- "42"
            Task.FromResult<obj>(null) :> Task)

    let converted = OwinAppFunc.toFreya app

    let m =
        freya {
            do! converted
            let! v1 = Freya.getLensPartial answerLens
            return Option.get v1 }
    
    let result = run m
    fst result =? "42"

[<Test>]
let ``freya computation can roundtrip to and from OwinAppFunc`` () =
    let app = Freya.setLensPartial answerLens "42"

    let converted =
        app
        |> OwinAppFunc.ofFreya
        |> OwinAppFunc.toFreya

    let m =
        freya {
            do! converted
            let! v1 = Freya.getLensPartial answerLens
            return Option.get v1 }
    
    let result = run m
    fst result =? "42"

(** MidFunc **)

open Freya.Core.Operators
open Freya.Pipeline
open Freya.Pipeline.Operators

let o1 =
    freya {
        do! Freya.setLens o1Lens true
        let! prev = Freya.getLensPartial answerLens
        do! Freya.setLensPartial answerLens (appendString "1" prev) }

let o2 =
    freya {
        do! Freya.setLens o2Lens true
        let! prev = Freya.getLensPartial answerLens
        do! Freya.setLensPartial answerLens (appendString "2" prev) }

let app =
    freya {
        let! prev = Freya.getLensPartial answerLens
        do! Freya.setLensPartial answerLens (appendString "42" prev) }
    |> OwinAppFunc.ofFreya

[<Test>]
let ``test app works`` () =
    let env = invoke app

    env.ContainsKey("Answer") =? true
    unbox env.["Answer"] =? "42"

[<Test>]
let ``pipeline executes both monads if first returns next`` () =
    let o1 = o1 *> next |> OwinMidFunc.ofFreya
    let o2 = o2 *> next |> OwinMidFunc.ofFreya
    let composed = o1.Invoke(o2.Invoke(app))

    let env = invoke composed

    unbox env.["o1"] =? true
    unbox env.["o2"] =? true
    unbox env.["Answer"] =? "1,2,42"

[<Test>]
let ``pipeline executes only the first monad if first returns halt`` () =
    let o1 = o1 *> halt |> OwinMidFunc.ofFreya
    let o2 = o2 *> next |> OwinMidFunc.ofFreya
    let composed = o1.Invoke(o2.Invoke(app))

    let env = invoke composed

    unbox env.["o1"] =? true
    unbox env.["o2"] =? false
    unbox env.["Answer"] =? "1"

[<Test>]
let ``pipeline executes app and both monads if first returns next after running the app`` () =
    let o1 = o1 *> next |> OwinMidFunc.ofFreyaAfter
    let o2 = o2 *> next |> OwinMidFunc.ofFreyaAfter
    let composed = o1.Invoke(o2.Invoke(app))

    let env = invoke composed

    unbox env.["o1"] =? true
    unbox env.["o2"] =? true
    unbox env.["Answer"] =? "42,2,1"

[<Test>]
let ``pipeline executes app and both monads if first returns halt after running the app`` () =
    let o1 = o1 *> halt |> OwinMidFunc.ofFreyaAfter
    let o2 = o2 *> next |> OwinMidFunc.ofFreyaAfter
    let composed = o1.Invoke(o2.Invoke(app))

    let env = invoke composed

    unbox env.["o1"] =? true
    unbox env.["o2"] =? true
    unbox env.["Answer"] =? "42,2,1"

[<Test>]
let ``pipeline executes both monads if first returns next with composed pipeline`` () =
    let o1 = o1 *> next
    let o2 = o2 *> next
    let pipe = o1 >?= o2 |> OwinMidFunc.ofFreya
    let composed = pipe.Invoke app

    let env = invoke composed

    unbox env.["o1"] =? true
    unbox env.["o2"] =? true
    unbox env.["Answer"] =? "1,2,42"

[<Test>]
let ``pipeline executes only the first monad if first returns terminate with composed pipeline`` () =
    let o1 = o1 *> halt
    let o2 = o2 *> next
    let pipe = o1 >?= o2 |> OwinMidFunc.ofFreya
    let composed = pipe.Invoke app

    let env = invoke composed

    unbox env.["o1"] =? true
    unbox env.["o2"] =? false
    unbox env.["Answer"] =? "1"

[<Test>]
let ``pipeline executes app and both monads if first returns next after running the app with composed pipeline`` () =
    let o1 = o1 *> next
    let o2 = o2 *> next
    let pipe = o1 >?= o2 |> OwinMidFunc.ofFreyaAfter
    let composed = pipe.Invoke app

    let env = invoke composed

    unbox env.["o1"] =? true
    unbox env.["o2"] =? true
    unbox env.["Answer"] =? "42,1,2"

[<Test>]
let ``pipeline executes app and only the first monad if first returns halt after running the app with composed pipeline`` () =
    let o1 = o1 *> halt
    let o2 = o2 *> next
    let pipe = o1 >?= o2 |> OwinMidFunc.ofFreyaAfter
    let composed = pipe.Invoke app

    let env = invoke composed

    unbox env.["o1"] =? true
    unbox env.["o2"] =? false
    unbox env.["Answer"] =? "42,1"

[<Test>]
let ``pipeline executes both monads if first returns next with wrapped pipeline OwinMidFunc`` () =
    let o1 = o1 *> next
    let o2 = o2 *> next
    let midFunc = OwinMidFunc.ofFreyaWrapped o1 o2
    let composed = midFunc.Invoke app

    let env = invoke composed

    unbox env.["o1"] =? true
    unbox env.["o2"] =? true
    unbox env.["Answer"] =? "1,42,2"

[<Test>]
let ``pipeline executes only the first monad if first returns terminate with wrapped pipeline OwinMidFunc`` () =
    let o1 = o1 *> halt
    let o2 = o2 *> next
    let midFunc = OwinMidFunc.ofFreyaWrapped o1 o2
    let composed = midFunc.Invoke app

    let env = invoke composed

    unbox env.["o1"] =? true
    unbox env.["o2"] =? false
    unbox env.["Answer"] =? "1"

[<Test>]
let ``MidFunc can roundtrip to Freya Pipeline before / after and back`` () =
    let midFunc =
        OwinMidFunc(fun next ->
            OwinAppFunc(fun env ->
                async {
                    env.["o3"] <- true
                    do! next.Invoke(env).ContinueWith<unit>(fun _ -> ()) |> Async.AwaitTask
                    env.["o4"] <- true
                } |> Async.StartAsTask :> Task ))

    let before, after = OwinMidFunc.splitIntoFreya midFunc
    let pipe = OwinMidFunc.ofFreyaWrapped before after
    let composed = pipe.Invoke app

    let env = invoke composed

    env.ContainsKey("Answer") =? true
    unbox env.["Answer"] =? "42"
    env.ContainsKey("o3") =? true
    unbox env.["o3"] =? true
    env.ContainsKey("o4") =? true
    unbox env.["o4"] =? true
