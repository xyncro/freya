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
module internal Freya.Machine.Http.Process

open Freya.Machine
open Freya.Machine.Operators

(* Process

   Decisions and topology of the section of an HTTP graph classified
   under Process, according to the HTTP decision diagram (v4.0.201410),
   defined as part of the for-GET project.

   See [https://github.com/for-GET/http-decision-diagram]. *)

// TODO: Modify this to better support a strict set of processable verbs.

(* Decisions *)

[<RequireQualifiedAccess>]
module Decisions =

    let [<Literal>] IsMethodDelete = "isMethodDelete"
    let [<Literal>] IsMethodHeadGet = "isMethodHeadGet"
    let [<Literal>] IsMethodProcess = "isMethodProcess"
    let [<Literal>] IsMethodPut = "isMethodPut"
    let [<Literal>] Process = "process"
    let [<Literal>] ProcessDelete = "processDelete"
    let [<Literal>] ProcessHasConflict = "processHasConflict"
    let [<Literal>] ProcessPartialPut = "processPartialPut"

    let isMethodDelete =
        decision IsMethodDelete false

    let isMethodHeadGet =
        decision IsMethodHeadGet true

    let isMethodProcess =
        decision IsMethodProcess true

    let isMethodPut =
        decision IsMethodPut true

    let proc =
        decision Process true

    let processDelete =
        decision ProcessDelete true

    let processHasConflict =
        decision ProcessHasConflict false

    let processPartialPut =
        decision ProcessPartialPut false

(* Graph *)

[<RequireQualifiedAccess>]
module Graph =

    let operations =
        [ Ref Decisions.IsMethodDelete                          =.        Binary Decisions.isMethodDelete
          Ref Decisions.IsMethodHeadGet                         =.        Binary Decisions.isMethodHeadGet
          Ref Decisions.IsMethodProcess                         =.        Binary Decisions.isMethodProcess
          Ref Decisions.IsMethodPut                             =.        Binary Decisions.isMethodPut
          Ref Decisions.Process                                 =.        Binary Decisions.proc
          Ref Decisions.ProcessDelete                           =.        Binary Decisions.processDelete
          Ref Decisions.ProcessHasConflict                      =.        Binary Decisions.processHasConflict
          Ref Decisions.ProcessPartialPut                       =.        Binary Decisions.processPartialPut
          
          Ref Precondition.Decisions.HasIfModifiedSince         >-        Ref Decisions.IsMethodHeadGet
          Ref Precondition.Decisions.IfModifiedSinceMatches     >+        Ref Decisions.IsMethodHeadGet
          Ref Precondition.Decisions.IfNoneMatchMatches         >+        Ref Decisions.IsMethodHeadGet
          Ref Decisions.IsMethodHeadGet                         >-        Ref Decisions.IsMethodDelete
          Ref Decisions.IsMethodDelete                          >+        Ref Decisions.ProcessDelete
          Ref Decisions.IsMethodDelete                          >-        Ref Decisions.IsMethodPut
          Ref Decisions.IsMethodPut                             >+        Ref Decisions.ProcessPartialPut
          Ref Decisions.IsMethodPut                             >-        Ref Decisions.IsMethodProcess
          Ref Decisions.IsMethodProcess                         >+        Ref Decisions.ProcessHasConflict
          Ref Decisions.IsMethodProcess                         >-        Ref Common.Operations.InternalServerError
          Ref Decisions.ProcessDelete                           >-        Ref Common.Operations.InternalServerError
          Ref Decisions.ProcessPartialPut                       >+        Ref Common.Operations.BadRequest
          Ref Decisions.ProcessPartialPut                       >-        Ref Decisions.ProcessHasConflict
          Ref Decisions.ProcessHasConflict                      >+        Ref Common.Operations.Conflict
          Ref Decisions.ProcessHasConflict                      >-        Ref Decisions.Process
          Ref Decisions.Process                                 >-        Ref Common.Operations.InternalServerError ]