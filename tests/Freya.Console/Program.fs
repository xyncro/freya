open Arachne.Http
open Freya.Core
open Freya.Core.Operators
open Freya.Lenses.Http
open Freya.Machine
open Freya.Machine.Extensions.Http
open Suave.Logging
open Suave.Owin
open Suave.Types
open Suave.Web

// Resources

let ok _ =
        Freya.Lens.setPartial Response.ReasonPhrase_ "Hey Folks!"
     *> Freya.init Representation.empty

let resource =
    freyaMachine {
        using http
        methodsSupported (Freya.init [ GET ])
        handleOk ok } |> FreyaMachine.toPipeline

// App

let config =
    { defaultConfig with
        bindings = [ HttpBinding.mk' HTTP "0.0.0.0" 7000 ]
        logger = Loggers.saneDefaultsFor LogLevel.Verbose }

// Main

[<EntryPoint>]
let main _ =

    printfn "Listening on port 7000"
    let owin = OwinApp.ofAppFunc "/" (OwinAppFunc.ofFreya resource)
    startWebServer config owin

    0
