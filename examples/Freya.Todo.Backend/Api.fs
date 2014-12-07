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
open Freya.Router.Inspector
open Freya.Types
open Freya.Types.Http
open Freya.Types.Language
open Freya.Todo.Backend.Storage

// Helpers

let inline represent (n: FreyaMachineNegotiation) x =
    { Metadata =
        { Charset = Some n.Charsets.Head
          Encodings = None
          MediaType = Some n.MediaTypes.Head
          Languages = Option.map (fun x -> [ List.head x ]) n.Languages }
      Data = toJSON x }

// Functions

let clearTodos =
    liftAsync (clear ())

let createTodo =
    returnM ()

let createdTodo n =
    represent n <!> returnM Array.empty<Todo>

let getTodos n =
    represent n <!> liftAsync (getAll ())

let todoProcessable =
    returnM true

let todoLastModified =
    returnM (DateTime.Now.Subtract (TimeSpan.FromDays 100.))

// Configuration

let en =
    returnM [ LanguageTag.Parse "en-GB"
              LanguageTag.Parse "en" ]

let json =
    returnM [ MediaType.JSON ]

let utf8 =
    returnM [ Charset.UTF8 ]

let defaults =
    freyaMachine {
        charsetsSupported utf8
        languagesSupported en
        mediaTypesSupported json }

// Resources

let todosMethods =
    returnM [ DELETE; GET; OPTIONS; POST ]

let todos =
    freyaMachine {
        including defaults

        doDelete clearTodos
        doPost createTodo

        handleCreated createdTodo
        handleOk getTodos

        lastModified todoLastModified
        methodsSupported todosMethods
        processable todoProcessable } |> compileFreyaMachine

let todo =
    freyaMachine {
        including defaults } |> compileFreyaMachine

// Routes

let api =
    freyaRouter {
        route All "/" todos
        route All "/:id" todo } |> compileFreyaRouter

// Pipeline

let config =
    { Path = "/inspect"
      Inspectors = 
        [ freyaMachineInspector
          freyaRouterInspector ] }

let pipeline =
    freyaInspector config >?= api

// Katana

type TodoBackend () =
    member __.Configuration () =
        OwinAppFunc.fromFreya (pipeline)