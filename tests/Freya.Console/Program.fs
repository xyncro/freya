open System.Text

// Freya (and Arachne)

open Arachne.Http
open Arachne.Language
open Freya.Core
open Freya.Core.Operators
open Freya.Lenses.Http
open Freya.Machine
open Freya.Machine.Extensions.Http
open Freya.Machine.Router
open Freya.Router

let en =
    LanguageTag.parse "en"

let inline represent (x: string) =
    { Description =
        { Charset = Some Charset.Utf8
          Encodings = None
          MediaType = Some MediaType.Text
          Languages = Some [ en ] }
      Data = Encoding.UTF8.GetBytes x }

let hello =
        Freya.Optic.set Response.reasonPhrase_ (Some "Hey Folks!")
     *> Freya.Optic.map Response.body_ (fun b ->
            let out = "Hey Folks!"B
            b.Write (out, 0, out.Length)
            b)
let ok =
    represent "Hey, folks!"

let common =
    freyaMachine {
        using http
        charsetsSupported Charset.Utf8
        languagesSupported en
        mediaTypesSupported MediaType.Text }

let home =
    freyaMachine {
        using http
        including common
        methodsSupported GET
        handleOk ok }

let routes =
    freyaRouter {
        resource "/" home
        resource "/hello" hello }

let app =
    OwinAppFunc.ofFreya routes

// Suave

open Suave.Http
open Suave.Logging
open Suave.Owin
open Suave.Web

let config =
    { defaultConfig with
        bindings = [ HttpBinding.mkSimple HTTP "0.0.0.0" 7000 ]
        logger = Loggers.saneDefaultsFor LogLevel.Verbose }

let suave () =
    startWebServer config (OwinApp.ofAppFunc "/" app)

// Katana

open Microsoft.Owin.Hosting

type Katana () =
    member __.Configuration () =
        app

let katana () =
    WebApp.Start<Katana> "http://localhost:7000"

// Main

[<EntryPoint>]
let main _ =

// Uncomment the wished for server!

//    let _ = katana ()
//    let _ = suave ()
    let _ = System.Console.ReadLine ()

    0