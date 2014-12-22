module Freya.Todo.Backend.Api

open System
open Freya.Core
open Freya.Core.Operators
open Freya.Machine
open Freya.Pipeline
open Freya.Router
open Freya.Types.Cors
open Freya.Types.Http
open Freya.Types.Language

// Core

let id =
    memoM ((Option.get >> Guid.Parse) <!> getPLM (Route.valuesKey "id"))

let add =
    memoM (asyncM Storage.add =<< (Option.get <!> body ()))

let clear =
    memoM (asyncM Storage.clear =<< returnM ())

let delete =
    memoM (asyncM Storage.delete =<< id)

let get =
    memoM (asyncM Storage.get =<< id)

let list =
    memoM (asyncM Storage.list =<< returnM ())

let update =
    memoM (asyncM Storage.update =<< (tuple <!> id <*> (Option.get <!> body ())))

// Machine

let addAction =
    ignore <!> add

let addedHandler _ =
    represent <!> add

let clearAction =
    ignore <!> clear

let deleteAction =
    ignore <!> delete

let getHandler _ =
    represent <!> get

let listHandler _ =
    represent <!> list

let updateAction =
    ignore <!> update

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

        corsMethodsSupported todosMethods
        methodsSupported todosMethods

        doDelete clearAction
        doPost addAction

        handleCreated addedHandler
        handleOk listHandler } |> compileFreyaMachine

let todoMethods =
    returnM [
        DELETE
        GET
        OPTIONS
        PATCH ]

let todo =
    freyaMachine {
        including defaults
        
        corsMethodsSupported todoMethods
        methodsSupported todoMethods

        doDelete deleteAction
        doPatch updateAction

        handleOk getHandler } |> compileFreyaMachine

// Routes

let todoBackend : FreyaPipeline =
    freyaRouter {
        route All "/" todos
        route All "/:id" todo } |> compileFreyaRouter