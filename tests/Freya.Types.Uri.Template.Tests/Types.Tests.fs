module Freya.Types.Uri.Templates.Tests

open NUnit.Framework
open Freya.Types.Uri.Template
open Swensen.Unquote

[<Test>]
let ``Level 1 Examples Render Correctly`` () =

    let data =
        UriTemplateData (
            Map.ofList [
                "var", Atom "value"
                "hello", Atom "Hello World!" ])

    UriTemplate.Parse("{var}").Render(data) =? "value"
    UriTemplate.Parse("{hello}").Render(data) =? "Hello%20World%21"