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