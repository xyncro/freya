module Freya.Typed.Tests.RFC7230

open NUnit.Framework
open Swensen.Unquote
open Freya.Typed

[<Test>]
let ``PartialUri Formatting/Parsing`` () =

    let partialUriTyped : PartialUri =
        { Relative = Absolute (PathAbsolute [ "some"; "path" ])
          Query = Some (Query "key=val") }

    let partialUriString =
        "/some/path?key=val"

    roundTrip (PartialUri.Format, PartialUri.Parse) [
        partialUriTyped, partialUriString ]

[<Test>]
let ``HttpVersion Formatting/Parsing`` () =
    
    let httpVersionTyped = HTTP 1.1
    let httpVersionString = "HTTP/1.1"

    roundTrip (HttpVersion.Format, HttpVersion.Parse) [
        httpVersionTyped, httpVersionString ]

[<Test>]
let ``ContentLength Formatting/Parsing`` () =
    
    let contentLengthTyped = ContentLength 1024
    let contentLengthString = "1024"

    roundTrip (ContentLength.Format, ContentLength.Parse) [
        contentLengthTyped, contentLengthString ]

[<Test>]
let ``Host Formatting/Parsing`` () =

    let hostTyped = Host (Name "www.example.com", Some (Port 8080))
    let hostString = "www.example.com:8080"

    roundTrip (Host.Format, Host.Parse) [
        hostTyped, hostString ]

[<Test>]
let ``Connection Formatting/Parsing`` () =

    let connectionTyped = Connection ([ ConnectionOption "close"; ConnectionOption "test" ])
    let connectionString = "close,test"

    roundTrip (Connection.Format, Connection.Parse) [
        connectionTyped, connectionString ]