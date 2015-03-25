open Freya.Router.New
open Freya.Types.Uri.Template
open Hekate

[<EntryPoint>]
let main _ =

    let root = UriTemplate.Parse "/"
    let hello = root + UriTemplate.Parse "hello"
    let location = hello + UriTemplate.Parse "/{location}"

    let data =
        [ root, 0
          hello, 1
          location, 2 ]

    let graph =
        create data

    let x = Graph.successors (RoutingKey "") graph

    0
