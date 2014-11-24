module Freya.Typed.Tests.RFC7233

open System
open NUnit.Framework
open Swensen.Unquote
open Freya.Typed

[<Test>]
let ``IfRange Formatting/Parsing`` () =

    let ifRangeTyped =
        IfRange (IfRangeChoice.Date (DateTime.Parse ("1994/10/29 19:43:31")))

    let ifRangeString =
        "Sat, 29 Oct 1994 19:43:31 GMT"

    roundTrip (IfRange.Format, IfRange.Parse) [
        ifRangeTyped, ifRangeString ]