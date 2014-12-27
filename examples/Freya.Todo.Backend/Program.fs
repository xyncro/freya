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

module Freya.Todo.Backend.Program

open Freya.Core
open Freya.Inspector
open Freya.Machine.Inspector
open Freya.Router.Inspector
open Freya.Pipeline.Operators
open Freya.Todo.Backend.Api
open Microsoft.Owin.Hosting

// App

let config =
    { Inspectors = 
        [ freyaRequestInspector
          freyaMachineInspector
          freyaRouterInspector ] }

let app =
    freyaInspector config >?= todoBackend

// Katana Compatible Type

type TodoBackend () =
    member __.Configuration () =
        OwinAppFunc.fromFreya (app)

// Main

[<EntryPoint>]
let main _ = 
    let _ = WebApp.Start<TodoBackend> ("http://localhost:7000")
    let _ = System.Console.ReadLine ()
    0