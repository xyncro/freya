open Microsoft.Owin.Hosting
open Arachne.Http
open Arachne.Language
open Arachne.Uri.Template
open Freya.Core
open Freya.Core.Operators
open Freya.Inspector
open Freya.Machine
open Freya.Machine.Extensions.Http
open Freya.Machine.Inspector
open Freya.Router
open Freya.Router.Inspector

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

// Pipeline

let config =
    { Inspectors =
        [ freyaMachineInspector
          freyaRouterInspector ] }

let pipeline =
        freyaInspector config
    >?= routes

// App

let app =
    OwinAppFunc.ofFreya pipeline

type App () =
    member __.Configuration () =
        app

// Main

[<EntryPoint>]
let main _ =

    let _ = WebApp.Start<App> "http://localhost:8080"
    let _ = System.Console.ReadLine ()

    0