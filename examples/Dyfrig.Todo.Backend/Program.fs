open Dyfrig.Core
open Dyfrig.Core.Operators
open Dyfrig.Http
open Dyfrig.Pipeline
open Dyfrig.Pipeline.Operators
open Microsoft.Owin.Hosting


// Data Model

type Todo =
    { Index: int
      Title: string
      Complete: bool }


// Data Operations

type TodoOperation =
    | Add of Todo
    | Get of int * AsyncReplyChannel<Todo option>

let todoStore = MailboxProcessor.Start (fun mailbox ->
    let rec loop storage =
        async {
            let! todoOperation = mailbox.Receive ()

            match todoOperation with
            | Add (todo) -> 
                return! loop (todo :: storage)
            | Get (index, chan) ->
                chan.Reply (List.tryFind (fun x -> x.Index = index) storage)
                return! loop storage }
                
    loop [])

let add todo =
    todoStore.Post (Add todo)

let get index =
    todoStore.PostAndAsyncReply (fun chan -> Get (index, chan))


// Dyfrig Pipeline

let cors =
    owin {
        do! setLM (Response.header "Access-Control-Allow-Origin") (Some [| "*" |])
        do! setLM (Response.header "Access-Control-Allow-Headers") (Some [| "Content-Type" |])

        return Proceed }

let todos =
    owin { return () } >>= proceed

let app =
    cors >?= todos


// OWIN Application (Katana style)

type TodoBackend () =
    member __.Configuration () =
        OwinAppFunc.fromOwinMonad (app)


// Main

[<EntryPoint>]
let main _ = 
    let _ = WebApp.Start<TodoBackend> ("http://*:7000")
    let _ = System.Console.ReadLine ()
    0
