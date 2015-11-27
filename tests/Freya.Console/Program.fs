open Arachne.Http
open Freya.Core
open Freya.Core.Operators
open Freya.Lenses.Http
open Freya.Machine
open Freya.Machine.Extensions.Http
open Freya.Machine.Router
open Freya.Router
open Microsoft.Owin.Hosting

// Resources

let ok =
        Freya.Lens.set Response.reasonPhrase_ (Some "Hey Folks!")
     *> Freya.init Representation.empty

let home =
    freyaMachine {
        using http
        methodsSupported GET
        handleOk ok }

// Routes

let routes =
    freyaRouter {
        resource "/" home }

// App

type App () =
    member __.Configuration () =
        OwinAppFunc.ofFreya routes

// Main

[<EntryPoint>]
let main _ =

    let _ = WebApp.Start<App> "http://localhost:8080"
    let _ = System.Console.ReadLine ()

    0