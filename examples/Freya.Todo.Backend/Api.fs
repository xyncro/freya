module Freya.Todo.Backend.Api

open System
open Freya.Core
open Freya.Core.Operators
open Freya.Inspector
open Freya.Machine
open Freya.Machine.Inspector
open Freya.Pipeline.Operators
open Freya.Router
open Freya.Router.Inspector
open Freya.Types.Cors
open Freya.Types.Http
open Freya.Types.Language
open Freya.Todo.Backend.Storage

// Freya

let newTodo =
    memoM (body ())

let clear =
    memoM (asyncM Storage.clear =<< returnM ())

let create =
    memoM (asyncM Storage.add =<< (Option.get <!> newTodo))

let list =
    memoM (asyncM Storage.list =<< returnM ())

// Machine

let clearAction =
    ignore <!> clear

let createAction =
    ignore <!> create

let createdHandler _ =
    represent <!> create

let listHandler _ =
    represent <!> list








// Defaults

let defaults =
    freyaMachine {
        charsetsSupported utf8
        corsHeadersSupported corsHeaders
        corsOriginsSupported corsOrigins
        languagesSupported en
        mediaTypesSupported json }

// Resources

let todosMethods =
    returnM [ 
        DELETE
        GET
        OPTIONS
        POST ]

let todos =
    freyaMachine {
        including defaults

        // Configuration

        corsMethodsSupported todosMethods
        methodsSupported todosMethods

        // Actions

        doDelete clearAction
        doPost createAction

        // Handlers

        handleCreated createdHandler
        handleOk listHandler } |> compileFreyaMachine

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
    { Inspectors = 
        [ freyaRequestInspector
          freyaMachineInspector
          freyaRouterInspector ] }

let pipeline =
    freyaInspector config >?= api

// Katana

type TodoBackend () =
    member __.Configuration () =
        OwinAppFunc.fromFreya (pipeline)