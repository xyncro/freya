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

open Freya.Machine
open Freya.Machine.Operators

(* Create

   Decisions and topology of the section of an HTTP graph classified
   under Create, according to the HTTP decision diagram (v4.0.201410),
   defined as part of the for-GET project.

   See [https://github.com/for-GET/http-decision-diagram].

   Delta/Notes:

   * createHasConflict transfers to create in the false case
     rather than the response block. The diagram is assumed to be wrong
     here, as this would imply that a valid put action will never be
     passed to create and thus never executed.

   * isMethodCreate has been translated to createIsMethodPost, as we
     only intend to support PUT or POST for creation in a standard HTTP
     graph. Custom verb support can be added by a graph extension
     if required. Though the defaults in the published docs imply PATCH
     as a valid creational verb, I would dispute this. *)

(* Decisions *)

[<RequireQualifiedAccess>]
module Decisions =

    let [<Literal>] Create = "create"
    let [<Literal>] CreateHasConflict = "createHasConflict"
    let [<Literal>] CreateIsMethodPost = "createIsMethodPost"
    let [<Literal>] CreateIsMethodPut = "createIsMethodPut"
    let [<Literal>] CreatePartialPut = "createPartialPut"
    let [<Literal>] CreatePath = "createPath"

    let create =
        decision Create false

    let createHasConflict =
        decision CreateHasConflict false

    let createIsMethodPost =
        decision CreateIsMethodPost false

    let createIsMethodPut =
        decision CreateIsMethodPut false

    let createPartialPut =
        decision CreatePartialPut false

    let createPath =
        decision CreatePath false

(* Graph *)

[<RequireQualifiedAccess>]
module Graph =

    let operations =
        [ Ref Decisions.Create                                  =.        Binary Decisions.create
          Ref Decisions.CreateHasConflict                       =.        Binary Decisions.createHasConflict
          Ref Decisions.CreateIsMethodPut                       =.        Binary Decisions.createIsMethodPut
          Ref Decisions.CreatePartialPut                        =.        Binary Decisions.createPartialPut
          Ref Decisions.CreatePath                              =.        Binary Decisions.createPath
          Ref Decisions.CreateIsMethodPost                      =.        Binary Decisions.createIsMethodPost
          
          Ref Retrieve.Decisions.Moved                          >-        Ref Decisions.CreateIsMethodPut
          Ref Retrieve.Decisions.GonePermanently                >-        Ref Decisions.CreateIsMethodPut
          Ref Decisions.CreateIsMethodPut                       >+        Ref Decisions.CreatePartialPut
          Ref Decisions.CreateIsMethodPut                       >-        Ref Decisions.CreateIsMethodPost
          Ref Decisions.CreateIsMethodPost                      >+        Ref Decisions.CreatePath
          Ref Decisions.CreateIsMethodPost                      >-        Ref Common.Operations.NotFound
          Ref Decisions.CreatePath                              >+        Ref Decisions.Create
          Ref Decisions.CreatePath                              >-        Ref Common.Operations.InternalServerError
          Ref Decisions.CreatePartialPut                        >+        Ref Common.Operations.BadRequest
          Ref Decisions.CreatePartialPut                        >-        Ref Decisions.CreateHasConflict
          Ref Decisions.Create                                  >-        Ref Common.Operations.InternalServerError
          Ref Decisions.CreateHasConflict                       >+        Ref Common.Operations.Conflict
          Ref Decisions.CreateHasConflict                       >-        Ref Decisions.Create ]