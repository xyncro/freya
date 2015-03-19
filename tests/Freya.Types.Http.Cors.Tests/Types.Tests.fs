module Freya.Types.Http.Cors.Tests

open System
open NUnit.Framework
open Freya.Types.Http
open Freya.Types.Http.Cors
open Freya.Types.Tests
open Freya.Types.Uri

[<Test>]
let ``Origin Formatting/Parsing`` () =
    let originTyped =
        Origin (
            OriginListOrNull.Origins [
                SerializedOrigin (
                    Scheme "http",
                    Name (RegName "www.example.com"),
                    Some (Port 8080)) ])

    let originString =
        "http://www.example.com:8080"

    roundTrip (Origin.Format, Origin.Parse) [
        originTyped, originString ]

[<Test>]
let ``AccessControlAllowOrigin Formatting/Parsing`` () =
    let accessControlAllowOriginTyped =
        AccessControlAllowOrigin (
            Origins (
                OriginListOrNull.Origins [
                    SerializedOrigin (
                        Scheme "http",
                        Name (RegName "www.example.com"),
                        Some (Port 8080)) ]))

    let accessControlAllowOriginString =
        "http://www.example.com:8080"

    roundTrip (AccessControlAllowOrigin.Format, AccessControlAllowOrigin.Parse) [
        accessControlAllowOriginTyped, accessControlAllowOriginString ]

[<Test>]
let ``AccessControlAllowCredentials Formatting/Parsing`` () =
    let accessControlAllowCredentialsTyped =
        AccessControlAllowCredentials

    let accessControlAllowCredentialsString =
        "true"

    roundTrip (AccessControlAllowCredentials.Format, AccessControlAllowCredentials.Parse) [
        accessControlAllowCredentialsTyped, accessControlAllowCredentialsString ]

[<Test>]
let ``AccessControlExposeHeaders Formatting/Parsing`` () =
    let accessControlExposeHeadersTyped =
        AccessControlExposeHeaders [ "X-Custom-Header"; "X-Another-Header" ]

    let accessControlExposeHeadersString =
        "X-Custom-Header,X-Another-Header"

    roundTrip (AccessControlExposeHeaders.Format, AccessControlExposeHeaders.Parse) [
        accessControlExposeHeadersTyped, accessControlExposeHeadersString ]

[<Test>]
let ``AccessControlMaxAge Formatting/Parsing`` () =
    let accessControlMaxAgeTyped =
        AccessControlMaxAge (TimeSpan.FromSeconds (1024.))

    let accessControlMaxAgeString =
        "1024"

    roundTrip (AccessControlMaxAge.Format, AccessControlMaxAge.Parse) [
        accessControlMaxAgeTyped, accessControlMaxAgeString ]

[<Test>]
let ``AccessControlAllowMethods Formatting/Parsing`` () =
    let accessControlAllowMethodsTyped =
        AccessControlAllowMethods [ DELETE; PUT ]

    let accessControlAllowMethodsString =
        "DELETE,PUT"

    roundTrip (AccessControlAllowMethods.Format, AccessControlAllowMethods.Parse) [
        accessControlAllowMethodsTyped, accessControlAllowMethodsString ]

[<Test>]
let ``AccessControlAllowHeaders Formatting/Parsing`` () =
    let accessControlAllowHeadersTyped =
        AccessControlAllowHeaders [ "X-Custom-Header"; "X-Another-Header" ]

    let accessControlAllowHeadersString =
        "X-Custom-Header,X-Another-Header"

    roundTrip (AccessControlAllowHeaders.Format, AccessControlAllowHeaders.Parse) [
        accessControlAllowHeadersTyped, accessControlAllowHeadersString ]

[<Test>]
let ``AccessControlRequestMethod Formatting/Parsing`` () =
    let accessControlRequestMethodTyped =
        AccessControlRequestMethod DELETE

    let accessControlRequestMethodString =
        "DELETE"

    roundTrip (AccessControlRequestMethod.Format, AccessControlRequestMethod.Parse) [
        accessControlRequestMethodTyped, accessControlRequestMethodString ]

[<Test>]
let ``AccessControlRequestHeaders Formatting/Parsing`` () =
    let accessControlRequestHeadersTyped =
        AccessControlRequestHeaders [ "X-Custom-Header"; "X-Another-Header" ]

    let accessControlRequestHeadersString =
        "X-Custom-Header,X-Another-Header"

    roundTrip (AccessControlRequestHeaders.Format, AccessControlRequestHeaders.Parse) [
        accessControlRequestHeadersTyped, accessControlRequestHeadersString ]