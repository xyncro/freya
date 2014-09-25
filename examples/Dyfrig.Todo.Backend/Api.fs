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
    serialize <!> liftAsync (getAll ())

let todoProcessable =
    owin {
        return true }

// Resources

let todos =
    machine {
        allowedMethods [ DELETE; GET; OPTIONS; POST ]
        doDelete clearTodos
        doPost createTodo
        handleCreated createdTodo
        handleOk getTodos
        processable todoProcessable } |> reifyMachine

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