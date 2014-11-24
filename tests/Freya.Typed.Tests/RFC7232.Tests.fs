module Freya.Typed.Tests.RFC7232

open System
open NUnit.Framework
open Swensen.Unquote
open Freya.Typed

[<Test>]
let ``LastModified Formatting/Parsing`` () =

    let lastModifiedTyped =
        LastModified (DateTime.Parse ("1994/10/29 19:43:31"))

    let lastModifiedString =
        "Sat, 29 Oct 1994 19:43:31 GMT"

    roundTrip (LastModified.Format, LastModified.Parse) [
        lastModifiedTyped, lastModifiedString ]

[<Test>]
let ``ETag Formatting/Parsing`` () =

    let eTagTyped =
        ETag (Strong "sometag")

    let eTagString =
        "\"sometag\""

    roundTrip (ETag.Format, ETag.Parse) [
        eTagTyped, eTagString ]

[<Test>]
let ``IfMatch Formatting/Parsing`` () =

    let ifMatchTyped =
        IfMatch (IfMatchChoice.EntityTags [ Strong "sometag"; Strong "othertag" ])

    let ifMatchString =
        "\"sometag\",\"othertag\""

    roundTrip (IfMatch.Format, IfMatch.Parse) [
        ifMatchTyped, ifMatchString ]

[<Test>]
let ``IfNoneMatch Formatting/Parsing`` () =

    let ifNoneMatchTyped =
        IfNoneMatch (IfNoneMatchChoice.EntityTags [ Strong "sometag"; Strong "othertag" ])

    let ifNoneMatchString =
        "\"sometag\",\"othertag\""

    roundTrip (IfNoneMatch.Format, IfNoneMatch.Parse) [
        ifNoneMatchTyped, ifNoneMatchString ]

[<Test>]
let ``IfModifiedSince Formatting/Parsing`` () =

    let ifModifiedSinceTyped =
        IfModifiedSince (DateTime.Parse ("1994/10/29 19:43:31"))

    let ifModifiedSinceString =
        "Sat, 29 Oct 1994 19:43:31 GMT"

    roundTrip (IfModifiedSince.Format, IfModifiedSince.Parse) [
        ifModifiedSinceTyped, ifModifiedSinceString ]

[<Test>]
let ``IfUnmodifiedSince Formatting/Parsing`` () =

    let ifUnmodifiedSinceTyped =
        IfUnmodifiedSince (DateTime.Parse ("1994/10/29 19:43:31"))

    let ifUnmodifiedSinceString =
        "Sat, 29 Oct 1994 19:43:31 GMT"

    roundTrip (IfUnmodifiedSince.Format, IfUnmodifiedSince.Parse) [
        ifUnmodifiedSinceTyped, ifUnmodifiedSinceString ]