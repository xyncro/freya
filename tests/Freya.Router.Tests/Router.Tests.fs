module Freya.Router.Tests.Router

open NUnit.Framework
open Swensen.Unquote
open Arachne.Http
open Arachne.Uri.Template
open Freya.Core
open Freya.Router

[<Test>]
let ``Router With No Routes Returns Next`` () =
    let routes =
        freyaRouter {
            return () }

    result GET "/" routes =! Next

[<Test>]
let ``Router With Base Route Executes Correctly`` () =
    let routes =
        freyaRouter {
            route All (UriTemplate.Parse "/") route1 }

    value GET "/" routes =! Some 1

[<Test>]
let ``Router With Multiple Routes Executes Correctly`` () =
    let routes =
        freyaRouter {
            route All (UriTemplate.Parse "/") route1
            route All (UriTemplate.Parse "/some/path") route2
            route All (UriTemplate.Parse "/other/path") route3 }

    value GET "/" routes =! Some 1
    value GET "/some/path" routes =! Some 2
    value GET "/other/path" routes =! Some 3
    value GET "/unset/path" routes =! None

[<Test>]
let ``Router With Method Specific Routes Executes Correctly`` () =
    let routes =
        freyaRouter {
            route Get (UriTemplate.Parse "/") route1
            route Get (UriTemplate.Parse "/some/path") route2
            route Post (UriTemplate.Parse "/some/path") route3 }

    value GET "/" routes =! Some 1
    value POST "/" routes =! None
    value GET "/some/path" routes =! Some 2
    value POST "/some/path" routes =! Some 3

[<Test>]
let ``Router Executes Pipeline Registered First`` () =
    let routes =
        freyaRouter {
            route Get (UriTemplate.Parse "/") route1
            route All (UriTemplate.Parse "/") route2 }

    value GET "/" routes =! Some 1