open Microsoft.Owin.Hosting
open Arachne.Http
open Freya.Core
open Freya.Core.Operators
open Freya.Lenses.Http
open Freya.Machine
open Freya.Machine.Extensions.Http

// Resources

let ok _ =
        Freya.Lens.set Response.reasonPhrase_ (Some "Hey Folks!")
     *> Freya.init Representation.empty

let resource =
    freyaMachine {
        using http
        methodsSupported (Freya.init [ GET ])
        handleOk ok } |> FreyaMachine.toPipeline

// App

let app =
    OwinAppFunc.ofFreya resource

type App () =
    member __.Configuration () =
        app

// Main

[<EntryPoint>]
let main _ =

    let _ = WebApp.Start<App> "http://localhost:8080"
    let _ = System.Console.ReadLine ()

    0