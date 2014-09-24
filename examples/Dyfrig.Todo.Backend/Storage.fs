module Dyfrig.Todo.Backend.Storage

open Fleece
open Fleece.Operators
open FSharpPlus

// Data Model

type Todo =
    { Index: int
      Title: string
      Complete: bool }

    static member Create title =
        { Index = 0
          Title = title
          Complete = false }

// JSON

    static member ToJSON (x: Todo) =
        jobj [
            "title" .= x.Title ]

    static member FromJson (_: Todo) =
        function
        | JObject o -> Todo.Create <!> (o .@ "title")
        | _ -> Failure ("")

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