module Dyfrig.Todo.Backend.Api

open System
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
    returnM ()

let createdTodo =
    returnM Array.empty

let getTodos =
    toJSON <!> liftAsync (getAll ())

let todoProcessable =
    returnM true

let todoLastModified =
    returnM (DateTime.Now.Subtract (TimeSpan.FromDays 100.))

// Resource Configuration

let json =
    machine {
        mediaTypesSupported [ 
            ClosedMediaRange (
                MediaType "application", 
                MediaSubType "json") ] }

let unicode =
    machine {
        charsetsSupported [ 
            SpecifiedCharset.Named "unicode-1-1" ] }

// Resources

let todos =
    machine {
        including json
        including unicode
        
        doDelete clearTodos
        doPost createTodo

        handleCreated createdTodo
        handleOk getTodos

        lastModified todoLastModified
        methodsSupported [ DELETE; GET; OPTIONS; POST ]
        processable todoProcessable } |> reifyMachine

let todo =
    machine {
        including json } |> reifyMachine

// Routes

let api =
    routes {
        route Any "/" todos
        route Any "/:id" todo } |> reifyRoutes

// Katana

type TodoBackend () =
    member __.Configuration () =
        OwinAppFunc.fromOwinMonad (api)