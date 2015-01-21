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
//
//----------------------------------------------------------------------------

module Freya.TodoBackend.Program

open Freya.Core
open Microsoft.Owin.Hosting

(* Katana

   Katana (Owin Self Hosting) expects us to expose a type with a specific
   method. Freya lets us do see easily, the OwinAppFunc module providing
   functions to turn any Freya<'a> function in to a suitable value for
   OWIN compatible hosts such as Katana. *)

type TodoBackend () =
    member __.Configuration () =
        OwinAppFunc.fromFreya (api)

(* Main

   A very simple program, simply a console app, with a blocking read from
   the console to keep our server from shutting down immediately. Though
   we are self hosting here as a console application, the same application
   should be easily transferrable to any OWIN compatible server, including
   IIS. *)

[<EntryPoint>]
let main _ = 
    let _ = WebApp.Start<TodoBackend> ("http://localhost:7000")
    let _ = System.Console.ReadLine ()
    0