module Freya.Router.Tests.Router

open NUnit.Framework
open Swensen.Unquote
open Freya.Pipeline
open Freya.Router
open Freya.Typed

[<Test>]
let ``Router With No Routes Returns Next`` () =
    let routes =
        freyaRouter {
            return () }

    result GET "/" routes =? Next

[<Test>]
let ``Router With Base Route Executes Correctly`` () =
    let routes =
        freyaRouter {
            route All "/" (set 1) }

    value GET "/" routes =? Some 1

[<Test>]
let ``Router With Multiple Routes Executes Correctly`` () =
    let routes =
        freyaRouter {
            route All "/" (set 1)
            route All "/some/path" (set 2)
            route All "/other/path" (set 3) }

    value GET "/" routes =? Some 1
    value GET "/some/path" routes =? Some 2
    value GET "/other/path" routes =? Some 3
    value GET "/unset/path" routes =? None

[<Test>]
let ``Router With Method Specific Routes Executes Correctly`` () =
    let routes =
        freyaRouter {
            route (Methods [ GET  ]) "/" (set 1)
            route (Methods [ GET  ]) "/some/path" (set 2)
            route (Methods [ POST ]) "/some/path" (set 3) }

    value GET "/" routes =? Some 1
    value POST "/" routes =? None
    value GET "/some/path" routes =? Some 2
    value POST "/some/path" routes =? Some 3

[<Test>]
let ``Router Executes Pipeline Registered First`` () =
    let routes =
        freyaRouter {
            route (Methods [ GET ]) "/" (set 1)
            route All "/" (set 2) }

    value GET "/" routes =? Some 1