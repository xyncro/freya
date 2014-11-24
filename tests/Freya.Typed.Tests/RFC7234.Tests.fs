module Freya.Typed.Tests.RFC7234

open System
open NUnit.Framework
open Swensen.Unquote
open Freya.Typed

[<Test>]
let ``Age Formatting/Parsing`` () =

    let ageTyped =
        Age (TimeSpan.FromSeconds (float 1024))

    let ageString =
        "1024"

    roundTrip (Age.Format, Age.Parse) [
        ageTyped, ageString ]

[<Test>]
let ``CacheControl Formatting/Parsing`` () =

    let cacheControlTyped =
        CacheControl [
            MaxAge (TimeSpan.FromSeconds (float 1024))
            NoCache
            Private ]

    let cacheControlString =
        "max-age=1024,no-cache,private"

    roundTrip (CacheControl.Format, CacheControl.Parse) [
        cacheControlTyped, cacheControlString ]

[<Test>]
let ``Expires Formatting/Parsing`` () =

    let expiresTyped =
        Expires (DateTime.Parse ("1994/10/29 19:43:31"))

    let expiresString =
        "Sat, 29 Oct 1994 19:43:31 GMT"

    roundTrip (Expires.Format, Expires.Parse) [
        expiresTyped, expiresString ]