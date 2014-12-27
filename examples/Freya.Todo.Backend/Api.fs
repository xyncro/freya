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
//----------------------------------------------------------------------------

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

// Freya

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