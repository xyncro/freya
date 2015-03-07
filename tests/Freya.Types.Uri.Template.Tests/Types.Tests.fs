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
                "name", Atom "andrew"
                "age",  Atom "34" ])

    let template = UriTemplate.Parse "/hello/{name,age}"
    let uri = template.TryRender data

    uri =? "/hello/andrew,34"