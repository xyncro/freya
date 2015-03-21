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

module Freya.TodoBackend.Api

open System
open Freya.Core
open Freya.Inspector
open Freya.Machine
open Freya.Machine.Extensions.Http
open Freya.Machine.Extensions.Http.Cors
open Freya.Machine.Inspector
open Freya.Machine.Router
open Freya.Pipeline.Operators
open Freya.Router
open Freya.Router.Inspector
open Freya.Types.Http
open Freya.Types.Http.Cors
open Freya.TodoBackend.Domain

(* Route Properties

   When designing Freya applications, particularly those based on machine,
   it's helpful to think in terms of *properties* of a request, rather than
   request data. This function, memoized, essentially represents a one-time
   computed *property* of the request (and specifically, in this case, the
   route).

   It might seem slightly unwise to be doing things like Option.get on values,
   but remember that we will only evaluate this property in the case of a route
   being matched which has an id, so it can be considered safe in this context. *)

let id =
    freya {
        let! id = Freya.getLensPartial (Route.valuesKey "id")
        return (Option.get >> Guid.Parse) id } |> Freya.memo

(* Body Properties

   As with the route properties, it is helpful to think of these values
   as properties of the request. Both newTodo and patchTodo are statically inferred,
   inferring the type to be returned from the context in which they're used. *)

let newTodo =
    Freya.memo (body ())

let patchTodo =
    Freya.memo (body ())

(* Domain Operations

   Here we can see that we wrap the domain api, turning the functions in to
   Freya<'a> functions using asyncM, and passing properties of the request
   to the functions.

   Again, we memoize the results as we don't need (or wish)
   to evaluate these more than once  per request - by memoizing here we can
   guarantee that these functions are idempotent within the scope of a
   request, allowing us to use them as part of multiple decisions safely. *)

let add =
    freya {
        let! newTodo = newTodo
        return! (Freya.fromAsync addTodo) newTodo.Value } |> Freya.memo

let clear =
    freya {
        return! (Freya.fromAsync clearTodos) () } |> Freya.memo

let delete =
    freya {
        let! id = id
        return! (Freya.fromAsync deleteTodo) id } |> Freya.memo

let get =
    freya {
        let! id = id
        return! (Freya.fromAsync getTodo) id } |> Freya.memo

let list =
    freya {
        return! (Freya.fromAsync listTodos) () } |> Freya.memo

let update =
    freya {
        let! id = id
        let! patchTodo = patchTodo
        return! (Freya.fromAsync updateTodo) (id, patchTodo.Value) } |> Freya.memo

(* Machine

   We define the functions that we'll use for decisions and resources
   within our freyaMachine expressions here. We can use the results of
   operations like "add" multiple times without worrying as we memoized
   that function.

   We also define a resource (common) of common properties of a resource,
   this saves us repeating configuration multiple times (once per resource).

   Finally we define our two resources, the first for the collection of Todos,
   the second for an individual Todo. *)

let addAction =
    freya {
        let! _ = add
        return () }

let addedHandler _ =
    freya {
        let! todo = add
        return represent todo }

let clearAction =
    freya {
        let! _ = clear
        return () }

let deleteAction =
    freya {
        let! _ = delete
        return () }

let getHandler _ =
    freya {
        let! todo = get
        return represent todo }

let listHandler _ =
    freya {
        let! todos = list
        return represent todos }

let updateAction =
    freya {
        let! _ = update
        return () }

let corsOrigins =
    freya {
        return AccessControlAllowOriginRange.Any }

let corsHeaders =
    freya {
        return [ "accept"
                 "content-type" ] }

let common =
    freyaMachine {
        using http
        using httpCors
        charsetsSupported utf8
        corsHeadersSupported corsHeaders
        corsOriginsSupported corsOrigins
        languagesSupported en
        mediaTypesSupported json }

let todosMethods =
    freya {
        return [ 
            DELETE
            GET
            OPTIONS
            POST ] }

let todos =
    freyaMachine {
        including common
        corsMethodsSupported todosMethods
        methodsSupported todosMethods
        doDelete clearAction
        doPost addAction
        handleCreated addedHandler
        handleOk listHandler } |> FreyaMachine.toPipeline

let todoMethods =
    freya {
        return [
            DELETE
            GET
            OPTIONS
            Method.Custom "PATCH" ] }

let todo =
    freyaMachine {
        including common
        corsMethodsSupported todoMethods
        methodsSupported todoMethods
        doDelete deleteAction
        doPatch updateAction
        handleOk getHandler } |> FreyaMachine.toPipeline

(* Router

   We have our two resources, but they need to have appropriate requests
   routed to them. We route them using the freyaRouter expression, using the
   shorthand "resource" syntax defined in Freya.Machine.Router (simply shorthand
   for "route All". *)

let todoRoutes =
    freyaRouter {
        resource "/" todos
        resource "/:id" todo } |> FreyaRouter.toPipeline

(* Inspectors *)

let config =
    { Inspectors = 
        [ freyaRequestInspector
          freyaMachineInspector
          freyaRouterInspector ] }

let inspect =
    freyaInspector config

(* API

   Finally we expose our actual API. In more complex applications than this
   we would expect to see multiple components of the application pipelined
   to form a more complex whole, but in this case we only have our single router. *)

let api =
    inspect >?= todoRoutes