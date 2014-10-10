module Dyfrig.Todo.Backend.Api

open System.Globalization
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

let json =
    machine {
        mediaTypesAvailable [ 
            ClosedMediaRange (
                MediaType "application", 
                MediaSubType "json") ] }

let unicode =
    machine {
        charsetsAvailable [ 
            NamedCharset "unicode-1-1" ] }

let todos =
    machine {
        including json
        including unicode
        
        methodsAllowed [ DELETE; GET; OPTIONS; POST ]
        doDelete clearTodos
        doPost createTodo
        handleCreated createdTodo
        handleOk getTodos
        requestProcessable todoProcessable } |> reifyMachine

let todo =
    machine {
        including json

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