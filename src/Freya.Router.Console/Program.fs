open Freya.Pipeline
open Freya.Router
open Freya.Types.Http
open Freya.Types.Uri.Template

[<EntryPoint>]
let main _ =

    let routes =
        [ { Method = Methods [ GET ]
            Specification = Path
            Template = UriTemplate.Parse "/hello/{location}"
            Pipeline = next }
          { Method = Methods [ POST ]
            Specification = Path
            Template = UriTemplate.Parse "/hello/{location}"
            Pipeline = next } ]

    let compiled = compile routes

    0