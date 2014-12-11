module Freya.Todo.Backend.Storage

open System
open System.Text
open Fleece
open Fleece.Operators
open FSharpPlus

// Models

type NewTodo =
    { Title: string
      Order: int option }

    static member FromJSON (_: NewTodo) =
        function | JObject o ->                    
                        (fun x o -> { Title = x
                                      Order = o }) 
                    <!> (o .@  "title")
                    <*> (o .@? "order") 
                 | x -> 
                    Failure ("Invalid NewTodo")

type Todo =
    { Id: Guid
      Url: string
      Order: int option
      Title: string
      Completed: bool }

    static member ToJSON (x: Todo) =
        jobj [
            "id" .= x.Id
            "url" .= x.Url
            "order" .= x.Order
            "title" .= x.Title
            "completed" .= x.Completed ]

// Storage

type StorageProtocol =
    | Add of NewTodo * AsyncReplyChannel<Todo>
    | Clear of AsyncReplyChannel<unit>
    | List of AsyncReplyChannel<Todo list>

type StorageState =
    { Todos: Map<Guid, Todo> }

let private add (x: NewTodo) (chan: AsyncReplyChannel<Todo>) state =
    let id =
        Guid.NewGuid ()
    
    let todo =
        { Id = id
          Url = sprintf "http://localhost:7000/%A" id
          Order = x.Order
          Title = x.Title
          Completed = false }

    chan.Reply (todo)

    { state with Todos = Map.add todo.Id todo state.Todos }

let private clear (chan: AsyncReplyChannel<unit>) state =
    chan.Reply()
    { state with Todos = Map.empty }

let private list (chan: AsyncReplyChannel<Todo list>) state =
    chan.Reply (state.Todos |> Map.toList |> List.map snd)
    state


let private makeStorage () = MailboxProcessor.Start (fun mbox ->
    let rec loop (state: StorageState) =
        async {
            let! protocol = mbox.Receive ()

            let action =
                match protocol with
                | Add (x, chan) -> add x chan
                | Clear (chan) -> clear chan
                | List (chan) -> list chan
                
            return! loop (action state) }

    loop { Todos = Map.empty })

let private store =
    makeStorage ()

// API

module Storage =

    open Freya.Core
    open Freya.Core.Operators

    let add x =
        store.PostAndAsyncReply (fun c -> Add (x, c))

    let clear () =
        store.PostAndAsyncReply (fun c -> Clear (c))

    let list () =
        store.PostAndAsyncReply (fun c -> List (c))