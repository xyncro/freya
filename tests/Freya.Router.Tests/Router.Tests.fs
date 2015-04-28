module Freya.Router.Tests.Router

open NUnit.Framework
open Swensen.Unquote
open Freya.Core.Pipeline
open Freya.Router
open Freya.Types.Http

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
            route All "/" route1 }

    value GET "/" routes =? Some 1

[<Test>]
let ``Router With Multiple Routes Executes Correctly`` () =
    let routes =
        freyaRouter {
            route All "/" route1
            route All "/some/path" route2
            route All "/other/path" route3 }

    value GET "/" routes =? Some 1
    value GET "/some/path" routes =? Some 2
    value GET "/other/path" routes =? Some 3
    value GET "/unset/path" routes =? None

[<Test>]
let ``Router With Method Specific Routes Executes Correctly`` () =
    let routes =
        freyaRouter {
            route Get "/" route1
            route Get "/some/path" route2
            route Post "/some/path" route3 }

    value GET "/" routes =? Some 1
    value POST "/" routes =? None
    value GET "/some/path" routes =? Some 2
    value POST "/some/path" routes =? Some 3

[<Test>]
let ``Router Executes Pipeline Registered First`` () =
    let routes =
        freyaRouter {
            route Get "/" route1
            route All "/" route2 }

    value GET "/" routes =? Some 1