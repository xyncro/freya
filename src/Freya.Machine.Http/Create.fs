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

[<RequireQualifiedAccess>]
module internal Freya.Machine.Http.Create

open Freya.Core
open Freya.Machine
open Freya.Machine.Operators

(* Create

   Decisions and topology of the section of an HTTP graph classified
   under Create, according to the HTTP decision diagram (v4.0.201410),
   defined as part of the for-GET project.

   See [https://github.com/for-GET/http-decision-diagram].

   Delta/Notes:

   * Topology changed to only support POST and PUT as create methods,
     as support for custom verbs should be added as a graph extension
     if required.

   * Create path decision removed as this is not sufficiently specified
     to implement at this stage.

   * Path checking for partial request and conflict now run for both PUT
     and POST, before branching to decide whether to run createPost or 
     createPut decision.

   * create decision has been split in to two separate decisions for a
     more granular end user experience, avoiding the potential need
     for the developer to check the current verb in a function to decide
     which logic to run in cases where POST and PUT logic differs.

   * Decisions removed:
     - create
     - createPath
     - createIsMethodPut
     - createPartialPut
     - isMethodCreate

   * Decisions added:
     - createhasConflict
     - createIsPost
     - createIsPut
     - createPartial
     - createPost
     - createPut
     - isMethodPostOrPut *)

(* Decisions *)

[<RequireQualifiedAccess>]
module Decisions =

    let [<Literal>] CreateHasConflict = "createHasConflict"
    let [<Literal>] CreateIsPost = "createIsPost"
    let [<Literal>] CreateIsPut = "createIsPut"
    let [<Literal>] CreatePartial = "createPartial"
    let [<Literal>] CreatePost = "createPost"
    let [<Literal>] CreatePut = "createPut"
    let [<Literal>] IsMethodPostOrPut = "isMethodPostOrPut"

    let createHasConflict =
        decision CreateHasConflict false

    let createIsPost _ =
        unconfigurable, Freya.init false

    let createIsPut _ =
        unconfigurable, Freya.init false

    let createPartial _ =
        unconfigurable, Freya.init false

    let createPost =
        decision CreatePost false

    let createPut =
        decision CreatePut false

    let isMethodPostOrPut _ =
        unconfigurable, Freya.init false

(* Graph *)

[<RequireQualifiedAccess>]
module Graph =

    let operations =
        [ Ref Decisions.CreateHasConflict                       =.        Binary Decisions.createHasConflict
          Ref Decisions.CreateIsPost                            =.        Binary Decisions.createIsPost
          Ref Decisions.CreateIsPut                             =.        Binary Decisions.createIsPut
          Ref Decisions.CreatePartial                           =.        Binary Decisions.createPartial
          Ref Decisions.CreatePost                              =.        Binary Decisions.createPost
          Ref Decisions.CreatePut                               =.        Binary Decisions.createPut
          Ref Decisions.IsMethodPostOrPut                       =.        Binary Decisions.isMethodPostOrPut
          
          Ref Retrieve.Decisions.Moved                          >-        Ref Decisions.IsMethodPostOrPut
          Ref Retrieve.Decisions.GonePermanently                >-        Ref Decisions.IsMethodPostOrPut
          Ref Decisions.IsMethodPostOrPut                       >+        Ref Decisions.CreatePartial
          Ref Decisions.IsMethodPostOrPut                       >-        Ref Common.Operations.NotFound
          Ref Decisions.CreatePartial                           >+        Ref Common.Operations.BadRequest
          Ref Decisions.CreatePartial                           >-        Ref Decisions.CreateHasConflict
          Ref Decisions.CreateHasConflict                       >+        Ref Common.Operations.Conflict
          Ref Decisions.CreateHasConflict                       >-        Ref Decisions.CreateIsPost
          Ref Decisions.CreateIsPost                            >+        Ref Decisions.CreatePost
          Ref Decisions.CreateIsPost                            >-        Ref Decisions.CreateIsPut
          Ref Decisions.CreatePost                              >-        Ref Common.Operations.InternalServerError
          Ref Decisions.CreateIsPut                             >+        Ref Decisions.CreatePut
          Ref Decisions.CreateIsPut                             >-        Ref Common.Operations.InternalServerError
          Ref Decisions.CreatePut                               >-        Ref Common.Operations.InternalServerError ]