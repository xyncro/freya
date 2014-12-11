[<AutoOpen>]
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
                        (fun t o -> { Title = t
                                      Order = o }) 
                    <!> (o .@  "title")
                    <*> (o .@? "order") 
                 | _ -> 
                    Failure ("Invalid NewTodo")

type PatchTodo =
    { Title: string option
      Order: int option
      Completed: bool option }

    static member FromJSON (_: PatchTodo) =
        function | JObject o ->
                        (fun t o c -> { Title = t
                                        Order = o
                                        Completed = c })
                    <!> (o .@? "title")
                    <*> (o .@? "order")
                    <*> (o .@? "completed")
                 | _ ->
                    Failure ("Invalid PatchTodo")

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
    | Delete of Guid * AsyncReplyChannel<unit>
    | Get of Guid * AsyncReplyChannel<Todo option>
    | List of AsyncReplyChannel<Todo list>
    | Update of Guid * PatchTodo * AsyncReplyChannel<Todo>

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

let private delete id (chan: AsyncReplyChannel<unit>) state =
    chan.Reply ()
    { state with Todos = Map.remove id state.Todos }

let private get id (chan: AsyncReplyChannel<Todo option>) state =
    chan.Reply(Map.tryFind id state.Todos)
    state

let private list (chan: AsyncReplyChannel<Todo list>) state =
    chan.Reply (state.Todos |> Map.toList |> List.map snd)
    state

let private update id (x: PatchTodo) (chan: AsyncReplyChannel<Todo>) state =
    let todo = Map.find id state.Todos

    let todo = (function | Some t -> { todo with Title = t } 
                         | _ -> todo) x.Title
    let todo = (function | Some o -> { todo with Order = Some o } 
                         | _ -> todo) x.Order
    let todo = (function | Some c -> { todo with Completed = c } 
                         | _ -> todo) x.Completed

    chan.Reply (todo)
    { state with Todos = Map.add id todo state.Todos }

let private makeStorage () = MailboxProcessor.Start (fun mbox ->
    let rec loop (state: StorageState) =
        async {
            let! protocol = mbox.Receive ()

            let action =
                match protocol with
                | Add (x, chan) -> add x chan
                | Clear (chan) -> clear chan
                | Delete (id, chan) -> delete id chan
                | Get (id, chan) -> get id chan
                | List (chan) -> list chan
                | Update (id, x, chan) -> update id x chan
                
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

    let delete id =
        store.PostAndAsyncReply (fun c -> Delete (id, c))

    let get id =
        store.PostAndAsyncReply (fun c -> Get (id, c))

    let list () =
        store.PostAndAsyncReply (fun c -> List (c))

    let update (id, x) =
        store.PostAndAsyncReply (fun c -> Update (id, x, c))