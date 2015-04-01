module Freya.Router.Tests.Router

open NUnit.Framework
open Swensen.Unquote
open Freya.Pipeline
open Freya.Router
open Freya.Types.Http
open Freya.Types.Uri.Template

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
            pathRoute All (UriTemplate.Parse "/") route1 }

    value GET "/" routes =? Some 1

[<Test>]
let ``Router With Multiple Routes Executes Correctly`` () =
    let routes =
        freyaRouter {
            pathRoute All (UriTemplate.Parse "/") route1
            pathRoute All (UriTemplate.Parse "/some/path") route2
            pathRoute All (UriTemplate.Parse "/other/path") route3 }

    value GET "/" routes =? Some 1
    value GET "/some/path" routes =? Some 2
    value GET "/other/path" routes =? Some 3
    value GET "/unset/path" routes =? None

[<Test>]
let ``Router With Method Specific Routes Executes Correctly`` () =
    let routes =
        freyaRouter {
            pathRoute Get (UriTemplate.Parse "/") route1
            pathRoute Get (UriTemplate.Parse "/some/path") route2
            pathRoute Post (UriTemplate.Parse "/some/path") route3 }

    //value GET "/" routes =? Some 1
    value POST "/" routes =? None
    //value GET "/some/path" routes =? Some 2
    //value POST "/some/path" routes =? Some 3

[<Test>]
let ``Router Executes Pipeline Registered First`` () =
    let routes =
        freyaRouter {
            pathRoute Get (UriTemplate.Parse "/") route1
            pathRoute All (UriTemplate.Parse "/") route2 }

    value GET "/" routes =? Some 1