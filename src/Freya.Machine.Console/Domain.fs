//----------------------------------------------------------------------------
//
// Copyright (c) 2014
//
//    Ryan Riley (@panesofglass) and Andrew Cherry (@kolektiv)
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
//----------------------------------------------------------------------------

[<AutoOpen>]
module Freya.TodoBackend.Domain

open System
open Aether
open Fleece
open Fleece.Operators
open FSharpPlus

(* Types

   We model our domain using some simple record types, including typed
   models for request bodies. Note that these only support serialization FROM
   JSON, while our Todo type itself only supports serialization TO JSON.

   This is quite a nice example of how having control over data at a fine
   grained level can be useful - there's no code anywhere that we don't need,
   and we can see exactly what's going on. *)

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

(* Constructors

   We often need constructor functions (or in this case, more
   accurately a mapping function) and they help keep code concise in examples
   such as this. *)

let todo (x: NewTodo) =
    let id = Guid.NewGuid ()

    { Id = id
      Url = sprintf "http://localhost:7000/%A" id
      Order = x.Order
      Title = x.Title
      Completed = false }

(* Storage

   In this case we'll model our actual store as a very simple in memory approach,
   using an F# MailboxProcessor. This actually gives us something quite close
   to what we'd encounter in a lot of real world projects - an asynchronous API
   that we need to use from our Freya specific code.

   We use a lens here for modification of our state, don't worry too much about
   the internals of this. How to build a small agent-like store isn't really the goal
   of this example! *)

type StorageState =
    { Todos: Map<Guid, Todo> }

    static member TodosLens =
        (fun x -> x.Todos), (fun t x -> { x with Todos = t })

type StorageProtocol =
    | Add of NewTodo * AsyncReplyChannel<Todo>
    | Clear of AsyncReplyChannel<unit>
    | Delete of Guid * AsyncReplyChannel<unit>
    | Get of Guid * AsyncReplyChannel<Todo option>
    | List of AsyncReplyChannel<Todo list>
    | Update of Guid * PatchTodo * AsyncReplyChannel<Todo>

let private reply (chan: AsyncReplyChannel<_>) x =
    chan.Reply (x); x

let private add chan newTodo =
    newTodo
    |> todo
    |> reply chan
    |> (fun todo -> modL StorageState.TodosLens (Map.add todo.Id todo))

let private clear chan =
    ()
    |> reply chan
    |> (fun () -> setL StorageState.TodosLens Map.empty)

let private delete chan id =
    ()
    |> reply chan
    |> (fun _ -> modL StorageState.TodosLens (Map.remove id))

let private get chan id state =
    getL StorageState.TodosLens state
    |> Map.tryFind id
    |> reply chan
    |> (fun _ -> state)

let private list chan state =
    getL StorageState.TodosLens state
    |> Map.toList
    |> List.map snd
    |> reply chan
    |> (fun _ -> state)

let private update (chan: AsyncReplyChannel<Todo>) id (patchTodo: PatchTodo) state =
    let todo = Map.find id state.Todos

    let todo = (function | Some t -> { todo with Title = t } 
                         | _ -> todo) patchTodo.Title
    let todo = (function | Some o -> { todo with Order = Some o } 
                         | _ -> todo) patchTodo.Order
    let todo = (function | Some c -> { todo with Completed = c } 
                         | _ -> todo) patchTodo.Completed

    chan.Reply (todo)
    { state with Todos = Map.add id todo state.Todos }

let private makeStorage () = 
    MailboxProcessor.Start (fun mbox ->
        let rec loop (state: StorageState) =
            async {
                let! protocol = mbox.Receive ()

                let action =
                    match protocol with
                    | Add (newTodo, chan) -> add chan newTodo
                    | Clear (chan) -> clear chan
                    | Delete (id, chan) -> delete chan id
                    | Get (id, chan) -> get chan id
                    | List (chan) -> list chan
                    | Update (id, patchTodo, chan) -> update chan id patchTodo
                
                return! loop (action state) }

        loop { Todos = Map.empty })

let private store =
    makeStorage ()

(* API

   Our actual storage API is a very simple collection of async functions
   which should be quite self explanatory. *)

let addTodo newTodo =
    store.PostAndAsyncReply (fun chan -> Add (newTodo, chan))

let clearTodos () =
    store.PostAndAsyncReply (fun chan -> Clear (chan))

let deleteTodo id =
    store.PostAndAsyncReply (fun chan -> Delete (id, chan))

let getTodo id =
    store.PostAndAsyncReply (fun chan -> Get (id, chan))

let listTodos () =
    store.PostAndAsyncReply (fun chan -> List (chan))

let updateTodo (id, patchTodo) =
    store.PostAndAsyncReply (fun chan -> Update (id, patchTodo, chan))