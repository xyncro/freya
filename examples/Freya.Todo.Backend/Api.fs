module Freya.Todo.Backend.Api

open System
open Freya.Core
open Freya.Core.Operators
open Freya.Inspector
open Freya.Machine
open Freya.Machine.Inspector
open Freya.Pipeline
open Freya.Pipeline.Operators
open Freya.Router
open Freya.Typed
open Freya.Todo.Backend.Storage

// Helpers

let asJSON =
    toJSON >> FreyaRepresentationResponse.Default

// Functions

let clearTodos =
    liftAsync (clear ())

let createTodo =
    returnM ()

let createdTodo _ =
    FreyaRepresentationResponse.Default <!> returnM Array.empty

let getTodos _ =
    asJSON <!> liftAsync (getAll ())

let todoProcessable =
    returnM true

let todoLastModified =
    returnM (DateTime.Now.Subtract (TimeSpan.FromDays 100.))

// Configuration

let json =
    freyaMachine {
        mediaTypesSupported [ MediaTypes.JSON ] }

let unicode =
    freyaMachine {
        charsetsSupported [ Charsets.Unicode ] }

// Resources

let todos =
    freyaMachine {
        including json
        including unicode

        doDelete clearTodos
        doPost createTodo

        handleCreated createdTodo
        handleOk getTodos

        lastModified todoLastModified
        methodsSupported [ DELETE; GET; OPTIONS; POST ]
        processable todoProcessable } |> compileFreyaMachine

let todo =
    freyaMachine {
        including json } |> compileFreyaMachine

// Routes

let api =
    freyaRouter {
        route All "/" todos
        route All "/:id" todo } |> compileFreyaRouter

// Pipeline

let config =
    { Path = "/inspect"
      History = 10
      Inspectors = [ freyaMachineInspector ] }

let pipeline =
    freyaInspector config >?= api

// Katana

type TodoBackend () =
    member __.Configuration () =
        OwinAppFunc.fromFreya (pipeline)