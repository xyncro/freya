open Microsoft.Owin.Hosting
open Arachne.Uri.Template
open Freya.Core
open Freya.Lenses.Http
open Freya.Router

// Handlers

let handler x =
    freya {
        do! Freya.setLensPartial Response.ReasonPhrase_ (sprintf "Handler %s" x)
        return! Freya.next }

// Routing

let routes = freyaRouter {
    route All (UriTemplate.Parse "/{location}/hello") (handler "A")
    route All (UriTemplate.Parse "/{place}/goodbye") (handler "B")
    route All (UriTemplate.Parse "/{location}/goodbye") (handler "C") } |> FreyaRouter.toPipeline

// App

let app =
    OwinAppFunc.ofFreya routes

type App () =
    member __.Configuration () =
        app

// Main

[<EntryPoint>]
let main _ =

    let _ = WebApp.Start<App> "http://localhost:8080"
    let _ = System.Console.ReadLine ()

    0