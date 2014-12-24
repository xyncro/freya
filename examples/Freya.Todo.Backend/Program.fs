module Freya.Todo.Backend.Program

open Freya.Core
open Microsoft.Owin.Hosting

// Katana Compatible Type

type TodoBackend () =
    member __.Configuration () =
        OwinAppFunc.fromFreya (api)

// Main

[<EntryPoint>]
let main _ = 
    let _ = WebApp.Start<TodoBackend> ("http://localhost:7000")
    let _ = System.Console.ReadLine ()
    0