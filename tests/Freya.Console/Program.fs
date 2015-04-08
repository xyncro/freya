open Freya.Core
open Freya.Machine
open Freya.Machine.Extensions.Http
open Freya.Router
open Freya.Types.Http
open Freya.Types.Language
open Freya.Types.Uri.Template
open Microsoft.Owin.Hosting

// Resources

let ok _ =
    freya {
        return {
            Data = "Hello World"B
            Description =
                { Charset = Some Charset.Utf8
                  Encodings = None
                  MediaType = Some MediaType.Text
                  Languages = Some [ LanguageTag.Parse "en-gb" ] } } }

let resource =
    freyaMachine {
        using http
        handleOk ok } |> FreyaMachine.toPipeline

// Routes

let routes =
    freyaRouter {
        route All (UriTemplate.Parse "/{id}") resource } |> FreyaRouter.toPipeline

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