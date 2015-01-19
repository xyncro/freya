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
module internal Freya.Machine.Http.Response

open Freya.Machine
open Freya.Machine.Operators

(* Process

   Decisions and topology of the section of an HTTP graph classified
   under Process, according to the HTTP decision diagram (v4.0.201410),
   defined as part of the for-GET project.

   See [https://github.com/for-GET/http-decision-diagram]. *)

// TODO: Better reflect create/process naming

(* Decisions *)

[<RequireQualifiedAccess>]
module Decisions =

    let [<Literal>] CreateSeeOther = "createSeeOther"
    let [<Literal>] HasMultipleChoices = "hasMultipleChoices"
    let [<Literal>] IsCreateDone = "isCreateDone"
    let [<Literal>] IsProcessDone = "isProcessDone"
    let [<Literal>] SeeOther = "seeOther"
    let [<Literal>] ToContent = "toContent"

    let createSeeOther =
        decision CreateSeeOther false

    let hasMultipleChoices =
        decision HasMultipleChoices false

    let isCreateDone =
        decision IsCreateDone true

    let isProcessDone =
        decision IsProcessDone true

    let seeOther =
        decision SeeOther false

    let toContent =
        decision ToContent true

(* Graph *)

[<RequireQualifiedAccess>]
module Graph =

    let operations =
        [ Ref Decisions.CreateSeeOther                          =.        Binary Decisions.createSeeOther
          Ref Decisions.HasMultipleChoices                      =.        Binary Decisions.hasMultipleChoices
          Ref Decisions.IsCreateDone                            =.        Binary Decisions.isCreateDone
          Ref Decisions.IsProcessDone                           =.        Binary Decisions.isProcessDone
          Ref Decisions.SeeOther                                =.        Binary Decisions.seeOther
          Ref Decisions.ToContent                               =.        Binary Decisions.toContent
          
          Ref Create.Decisions.CreatePost                       >+        Ref Decisions.IsCreateDone
          Ref Create.Decisions.CreatePut                        >+        Ref Decisions.IsCreateDone
          Ref Process.Decisions.IsMethodHeadGet                 >+        Ref Decisions.SeeOther
          Ref Process.Decisions.ProcessDelete                   >+        Ref Decisions.IsProcessDone
          Ref Process.Decisions.Process                         >+        Ref Decisions.IsProcessDone
          Ref Decisions.IsCreateDone                            >+        Ref Decisions.CreateSeeOther
          Ref Decisions.IsCreateDone                            >-        Ref Common.Operations.Accepted
          Ref Decisions.CreateSeeOther                          >+        Ref Common.Operations.SeeOther
          Ref Decisions.CreateSeeOther                          >-        Ref Common.Operations.Created
          Ref Decisions.ToContent                               >+        Ref Common.Operations.Ok
          Ref Decisions.ToContent                               >-        Ref Common.Operations.NoContent
          Ref Decisions.HasMultipleChoices                      >+        Ref Common.Operations.MultipleChoices
          Ref Decisions.HasMultipleChoices                      >-        Ref Decisions.ToContent
          Ref Decisions.SeeOther                                >+        Ref Common.Operations.SeeOther
          Ref Decisions.SeeOther                                >-        Ref Decisions.HasMultipleChoices
          Ref Decisions.IsProcessDone                           >+        Ref Decisions.SeeOther
          Ref Decisions.IsProcessDone                           >-        Ref Common.Operations.Accepted ]