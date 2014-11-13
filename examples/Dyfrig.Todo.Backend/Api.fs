module Dyfrig.Todo.Backend.Api

open System
open Dyfrig.Core
open Dyfrig.Core.Operators
open Dyfrig.Http
open Dyfrig.Inspector
open Dyfrig.Machine
open Dyfrig.Machine.Inspector
open Dyfrig.Pipeline
open Dyfrig.Pipeline.Operators
open Dyfrig.Router
open Dyfrig.Todo.Backend.Storage

// Helpers

let asJSON =
    toJSON >> RepresentationResponse.Default

// Functions

let clearTodos =
    liftAsync (clear ())

let createTodo =
    returnM ()

let createdTodo _ =
    RepresentationResponse.Default <!> returnM Array.empty

let getTodos _ =
    asJSON <!> liftAsync (getAll ())

let todoProcessable =
    returnM true

let todoLastModified =
    returnM (DateTime.Now.Subtract (TimeSpan.FromDays 100.))

// Configuration

let json =
    machine {
        mediaTypesSupported [ MediaTypes.JSON ] }

let unicode =
    machine {
        charsetsSupported [ Charsets.Unicode ] }

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

// Pipeline

let config =
    { Path = "/inspect"
      History = 10
      Inspectors = [ machineInspector ] }

let pipeline =
    inspector config >?= api

// Katana

type TodoBackend () =
    member __.Configuration () =
        OwinAppFunc.fromOwinMonad (pipeline)
