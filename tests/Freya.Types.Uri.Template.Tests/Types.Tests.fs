module Freya.Types.Uri.Templates.Tests

open NUnit.Framework
open Freya.Types.Tests
open Freya.Types.Uri.Template
open Swensen.Unquote

[<Test>]
let ``UriTemplate Formatting/Parsing`` () =
    let data =
        UriTemplateData (
            Map.ofList [
                "name", Atom "andrew" ])

    let x = UriTemplate.Parse "/hello/{name}"
    let u = x.TryRender data

    u =? "/hello/andrew"