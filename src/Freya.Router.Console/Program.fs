open Freya.Router
open Freya.Types.Uri.Template

[<EntryPoint>]
let main _ =

    let root = UriTemplate.Parse "/"
    let hello = root + UriTemplate.Parse "hello"
    let location = hello + UriTemplate.Parse "/{location}"

    let data =
        [ root, 0
          hello, 1
          location, 2 ]

    let data2 =
        [ UriTemplate.Parse "{greeting,name}", 0 ]

    let graph =
        compile data

    let res =
        executeCompilation graph "/hello/brighton"

    0
