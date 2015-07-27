open Microsoft.Owin.Hosting
open Arachne.Uri.Template
open Freya.Core
open Freya.Router

// Handlers

let handlerA =
    freya {
        return! Freya.next }

let handlerB =
    freya {
        return! Freya.next }

let handlerC =
    freya {
        return! Freya.next }

// Routing

let routes = freyaRouter {
    route All (UriTemplate.Parse "/{location}/hello") handlerA
    route All (UriTemplate.Parse "/{place}/goodbye") handlerB
    route All (UriTemplate.Parse "/{location}/goodbye") handlerC } |> FreyaRouter.toPipeline

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