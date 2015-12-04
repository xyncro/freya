open System.Text
open Arachne.Http
open Arachne.Language
open Arachne.Http
open Freya.Core
open Freya.Core.Operators
open Freya.Inspector
open Freya.Lenses.Http
open Freya.Machine
open Freya.Machine.Extensions.Http
open Freya.Machine.Inspector
open Freya.Machine.Router
open Freya.Router
open Freya.Router.Inspector
open Suave.Logging
open Suave.Owin
open Suave.Types
open Suave.Web

(* Resources *)

let en = LanguageTag.Parse "en"

let inline represent (x : string) =
    { Description =
        { Charset = Some Charset.Utf8
          Encodings = None
          MediaType = Some MediaType.Text
          Languages = Some [ en ] }
      Data = Encoding.UTF8.GetBytes x }

let ok =
        Freya.Optic.set Response.reasonPhrase_ (Some "Hey Folks!")
     *> Freya.init (represent "Hey, folks!")

let common =
    freyaMachine {
        using http
        charsetsSupported Charset.Utf8
        languagesSupported en
        mediaTypesSupported MediaType.Text }

let home =
    freyaMachine {
        using http
        //including common
        methodsSupported GET
        handleOk ok }

let routes =
    freyaRouter {
        resource "/" home }

(* Inspectors *)

let inspectorConfig =
    { Inspectors =
        [ freyaMachineInspector
          freyaRouterInspector ] }

let inspect =
    freyaInspector inspectorConfig

(* API *)

let api =
    inspect >?= routes

(* Suave *)

let config =
    { defaultConfig with
        bindings = [ HttpBinding.mk' HTTP "0.0.0.0" 7000 ]
        logger = Loggers.saneDefaultsFor LogLevel.Verbose }

let owin = OwinApp.ofAppFunc "/" (OwinAppFunc.ofFreya api)

// Main

[<EntryPoint>]
let main _ =

    printfn "Listening on port 7000"
    startWebServer config owin

    0
