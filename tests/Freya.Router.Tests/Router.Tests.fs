module Freya.Router.Tests.Router

open NUnit.Framework
open Swensen.Unquote
open Arachne.Http
open Arachne
open Arachne.Uri.Template
open Freya.Core
open Freya.Router

let private emptyQuery =
    Uri.Query ""

[<Test>]
let ``Router With No Routes Returns Next`` () =
    let routes =
        freyaRouter {
            return () }

    result GET "/" emptyQuery routes =! Next

[<Test>]
let ``Router With Base Route Executes Correctly`` () =
    let routes =
        freyaRouter {
            route All "/" route1 }

    value GET "/" emptyQuery routes =! Some 1

[<Test>]
let ``Router With Multiple Routes Executes Correctly`` () =
    let routes =
        freyaRouter {
            route All "/" route1
            route All "/some/path" route2
            route All "/other/path" route3 }

    value GET "/" emptyQuery routes =! Some 1
    value GET "/some/path" emptyQuery routes =! Some 2
    value GET "/other/path" emptyQuery routes =! Some 3
    value GET "/unset/path" emptyQuery routes =! None

[<Test>]
let ``Router With Method Specific Routes Executes Correctly`` () =
    let routes =
        freyaRouter {
            route GET "/" route1
            route GET "/some/path" route2
            route POST "/some/path" route3 }

    value GET "/" emptyQuery routes =! Some 1
    value POST "/" emptyQuery routes =! None
    value GET "/some/path" emptyQuery routes =! Some 2
    value POST "/some/path" emptyQuery routes =! Some 3

[<Test>]
let ``Router Executes Pipeline Registered First`` () =
    let routes =
        freyaRouter {
            route GET "/" route1
            route All "/" route2 }

    value GET "/" emptyQuery routes =! Some 1

[<Test>]
let ``Router Executes First Full Match`` () =
    let routes =
        freyaRouter {
            route All "/{one}/a" route1
            route All "/{two}/b" route2
            route All "/{one}/b" route3 }

    value GET "/some/b" emptyQuery routes =! Some 2