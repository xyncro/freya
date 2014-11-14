module Freya.Todo.Backend.Storage

open System.Text
open Fleece
open Fleece.Operators
open FSharpPlus

// Data Model

type Todo =
    { Index: int
      Title: string
      Complete: bool option }

    static member Create title =
        { Index = 0
          Title = title
          Complete = Some false }

    static member ToJSON (x: Todo) =
        jobj [
            "title" .= x.Title ]

    static member FromJson (_: Todo) =
        function
        | JObject o -> Todo.Create <!> (o .@ "title")
        | _ -> Failure ("")

// Serialization

let inline toJSON x =
    toJSON x
    |> string 
    |> Encoding.UTF8.GetBytes

// Data Operations

type private Operation =
    | Add of Todo
    | Clear of AsyncReplyChannel<unit>
    | Get of int * AsyncReplyChannel<Todo option>
    | GetAll of AsyncReplyChannel<Todo list>

let private store = MailboxProcessor.Start (fun mailbox ->
    let rec loop storage =
        async {
            let! operation = mailbox.Receive ()

            match operation with
            | Add (todo) -> 
                return! loop (todo :: storage)
            | Clear (c) ->
                c.Reply ()
                return! loop []
            | Get (index, c) ->
                c.Reply (List.tryFind (fun x -> x.Index = index) storage)
                return! loop storage
            | GetAll (c) ->
                c.Reply (storage)
                return! loop storage }
                
    loop [])

let add todo =
    store.Post (Add todo)

let clear () =
    store.PostAndAsyncReply (fun c -> Clear (c))

let get index =
    store.PostAndAsyncReply (fun c -> Get (index, c))

let getAll () =
    store.PostAndAsyncReply (fun c -> GetAll (c))
