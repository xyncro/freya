module Freya.Router.Tests.Router

open NUnit.Framework
open Swensen.Unquote
open Freya.Pipeline
open Freya.Router

[<Test>]
let ``Router With No Routes Returns Next`` () =
    let routes =
        freyaRouter {
            return () }

    result "/" routes =? Next

[<Test>]
let ``Router With Base Route Executes Correctly`` () =
    let routes =
        freyaRouter {
            route All "/" (set 1) }

    value "/" routes =? Some 1

[<Test>]
let ``Router With Multiple Routes Executes Correctly`` () =
    let routes =
        freyaRouter {
            route All "/" (set 1)
            route All "/some/path" (set 2)
            route All "/other/path" (set 3) }

    value "/" routes =? Some 1
    value "/some/path" routes =? Some 2
    value "/other/path" routes =? Some 3
    value "/unset/path" routes =? None