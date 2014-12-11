module Freya.Todo.Backend.Program

open Freya.Core
open Freya.Inspector
open Freya.Machine.Inspector
open Freya.Router.Inspector
open Freya.Pipeline.Operators
open Freya.Todo.Backend.Api
open Microsoft.Owin.Hosting

// App

let config =
    { Inspectors = 
        [ freyaRequestInspector
          freyaMachineInspector
          freyaRouterInspector ] }

let app =
    freyaInspector config >?= todoBackend

// Katana Compatible Type

type TodoBackend () =
    member __.Configuration () =
        OwinAppFunc.fromFreya (app)

// Main

[<EntryPoint>]
let main _ = 
    let _ = WebApp.Start<TodoBackend> ("http://localhost:7000")
    let _ = System.Console.ReadLine ()
    0