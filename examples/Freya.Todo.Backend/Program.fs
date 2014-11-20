module Freya.Todo.Backend.Program

open Freya.Todo.Backend.Api
open Microsoft.Owin.Hosting

// Main

[<EntryPoint>]
let main _ = 
    let _ = WebApp.Start<TodoBackend> ("http://localhost:7000")
    let _ = System.Console.ReadLine ()
    0
