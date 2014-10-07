module Dyfrig.Todo.Backend.Api

open Dyfrig.Core
open Dyfrig.Core.Operators
open Dyfrig.Http
open Dyfrig.Machine
open Dyfrig.Router
open Dyfrig.Todo.Backend.Storage

// Machine Functions

let clearTodos =
    liftAsync (clear ())

let createTodo =
    owin {
        return () }

let createdTodo =
    owin {
        return Array.empty }

let getTodos =
    toJSON <!> liftAsync (getAll ())

let todoProcessable =
    owin {
        return true }

// Resources

let todos =
    machine {
        methodsAllowed [ DELETE; GET; OPTIONS; POST ]
        doDelete clearTodos
        doPost createTodo
        handleCreated createdTodo
        handleOk getTodos
        requestProcessable todoProcessable } |> reifyMachine

let todo =
    machine {
        return () } |> reifyMachine

// Routes

let api =
    routes {
        route Any "/" todos
        route Any "/:id" todo } |> reifyRoutes

// Katana

type TodoBackend () =
    member __.Configuration () =
        OwinAppFunc.fromOwinMonad (api)