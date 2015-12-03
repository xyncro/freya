open System.Text
open Arachne.Http
open Arachne.Language
open Arachne.Http
open Freya.Core
open Freya.Core.Operators
open Freya.Lenses.Http
open Freya.Machine
open Freya.Machine.Extensions.Http
open Freya.Machine.Router
open Freya.Router
open Suave.Logging
open Suave.Owin
open Suave.Types
open Suave.Web

// Resources

let inline represent (x : string) =
    { Description =
        { Charset = Some Charset.Utf8
          Encodings = None
          MediaType = Some MediaType.Text
          Languages = Some [ LanguageTag.Parse "en" ] }
      Data = Encoding.UTF8.GetBytes x }

let ok =
    Freya.Optic.set Response.reasonPhrase_ (Some "Hey Folks!")
    *> Freya.init (represent "Hey, folks!")

let home =
    freyaMachine {
        using http
        methodsSupported GET
        handleOk ok }

let routes =
    freyaRouter {
        resource "/" home }

let config =
    { defaultConfig with
        bindings = [ HttpBinding.mk' HTTP "0.0.0.0" 7000 ]
        logger = Loggers.saneDefaultsFor LogLevel.Verbose }

let owin = OwinApp.ofAppFunc "/" (OwinAppFunc.ofFreya routes)

// Main

[<EntryPoint>]
let main _ =

    printfn "Listening on port 7000"
    startWebServer config owin

    0
