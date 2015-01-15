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
module internal Freya.Machine.Http.Request

open Freya.Core
open Freya.Machine
open Freya.Machine.Operators

(* System

   Decisions and topology of the section of an HTTP graph classified
   under System, according to the HTTP decision diagram (v4.0.201410),
   defined as part of the for-GET project.

   See [https://github.com/for-GET/http-decision-diagram].

   Delta/Notes:

   * isRequestBlockOk has been renamed to isRequestOk, to eliminate
     diagram related terminology from the eventual grammar.*)

(* Decisions *)

[<RequireQualifiedAccess>]
module Decisions =

    let [<Literal>] ExpectsContinue = "expectsContinue"
    let [<Literal>] FromContent = "fromContent"
    let [<Literal>] HasContent = "hasContent"
    let [<Literal>] IsAuthorized = "isAuthorized"
    let [<Literal>] IsContentTooLarge = "isContentTooLarge"
    let [<Literal>] IsContentTypeAccepted = "isContentTypeAccepted"
    let [<Literal>] IsForbidden = "isForbidden"
    let [<Literal>] IsMethodAllowed = "isMethodAllowed"
    let [<Literal>] IsMethodOptions = "isMethodOptions"
    let [<Literal>] IsMethodTrace = "isMethodTrace"
    let [<Literal>] IsRequestOk = "isRequestOk"

    let expectsContinue _ =
        unconfigurable, Freya.init false

    let fromContent =
        decision FromContent true

    let hasContent _ =
        unconfigurable, Freya.init true

    let isAuthorized =
        decision IsAuthorized true

    let isContentTooLarge _ =
        unconfigurable, Freya.init false

    let isContentTypeAccepted _ =
        unconfigurable, Freya.init true

    let isForbidden =
        decision IsForbidden false

    let isMethodAllowed _ =
        unconfigurable, Freya.init true

    let isMethodOptions _ =
        unconfigurable, Freya.init false

    let isMethodTrace _ =
        unconfigurable, Freya.init false

    let isRequestOk =
        decision IsRequestOk true

(* Graph *)

[<RequireQualifiedAccess>]
module Graph =

    let operations =
        [ Ref Decisions.ExpectsContinue                         =.        Binary Decisions.expectsContinue
          Ref Decisions.FromContent                             =.        Binary Decisions.fromContent
          Ref Decisions.HasContent                              =.        Binary Decisions.hasContent
          Ref Decisions.IsAuthorized                            =.        Binary Decisions.isAuthorized
          Ref Decisions.IsContentTooLarge                       =.        Binary Decisions.isContentTooLarge
          Ref Decisions.IsContentTypeAccepted                   =.        Binary Decisions.isContentTypeAccepted
          Ref Decisions.IsForbidden                             =.        Binary Decisions.isForbidden
          Ref Decisions.IsMethodAllowed                         =.        Binary Decisions.isMethodAllowed
          Ref Decisions.IsMethodOptions                         =.        Binary Decisions.isMethodOptions
          Ref Decisions.IsMethodTrace                           =.        Binary Decisions.isMethodTrace
          Ref Decisions.IsRequestOk                             =.        Binary Decisions.isRequestOk

          Ref System.Decisions.IsSystemOk                       >+        Ref Decisions.IsMethodAllowed
          Ref Decisions.IsMethodAllowed                         >+        Ref Decisions.IsAuthorized
          Ref Decisions.IsMethodAllowed                         >-        Ref Common.Operations.MethodNotAllowed
          Ref Decisions.IsAuthorized                            >+        Ref Decisions.ExpectsContinue
          Ref Decisions.IsAuthorized                            >-        Ref Common.Operations.Unauthorized
          Ref Decisions.ExpectsContinue                         >+        Ref Common.Operations.Continue
          Ref Decisions.ExpectsContinue                         >-        Ref Decisions.HasContent
          Ref Decisions.HasContent                              >+        Ref Decisions.IsContentTooLarge
          Ref Decisions.HasContent                              >-        Ref Decisions.IsForbidden
          Ref Decisions.IsContentTooLarge                       >+        Ref Common.Operations.PayloadTooLarge
          Ref Decisions.IsContentTooLarge                       >-        Ref Decisions.IsContentTypeAccepted
          Ref Decisions.IsContentTypeAccepted                   >+        Ref Decisions.FromContent
          Ref Decisions.IsContentTypeAccepted                   >-        Ref Common.Operations.UnsupportedMediaType
          Ref Decisions.FromContent                             >+        Ref Decisions.IsForbidden
          Ref Decisions.FromContent                             >-        Ref Common.Operations.BadRequest
          Ref Decisions.IsForbidden                             >+        Ref Common.Operations.Forbidden
          Ref Decisions.IsForbidden                             >-        Ref Decisions.IsMethodTrace
          Ref Decisions.IsMethodTrace                           >+        Ref Common.Operations.Ok
          Ref Decisions.IsMethodTrace                           >-        Ref Decisions.IsMethodOptions
          Ref Decisions.IsMethodOptions                         >+        Ref Common.Operations.Ok
          Ref Decisions.IsMethodOptions                         >-        Ref Decisions.IsRequestOk
          Ref Decisions.IsRequestOk                             >-        Ref Common.Operations.BadRequest ]