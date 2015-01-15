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
module internal Freya.Machine.Http.System

open Freya.Core
open Freya.Machine
open Freya.Machine.Operators

(* System

   Decisions and topology of the section of an HTTP graph classified
   under System, according to the HTTP decision diagram (v4.0.201410),
   defined as part of the for-GET project.

   See [https://github.com/for-GET/http-decision-diagram].

   Delta/Notes:

   * isSystemBlockOk has been renamed to isSystemOk, to eliminate
     diagram related terminology from the eventual grammar. *)

(* Decisions *)

[<RequireQualifiedAccess>]
module Decisions =

    let [<Literal>] AreContentHeadersImplemented = "areContentHeadersImplemented"
    let [<Literal>] AreExpectExtensionsImplemented = "areExpectExtensionsImplemented"
    let [<Literal>] AreHeadersTooLarge = "areHeadersTooLarge"
    let [<Literal>] IsFunctionalityImplemented = "isFunctionalityImplemented"
    let [<Literal>] IsMethodImplemented = "isMethodImplemented"
    let [<Literal>] IsServiceAvailable = "isServiceAvailable"
    let [<Literal>] IsSystemOk = "isSystemOk"
    let [<Literal>] IsUriTooLong = "isUriTooLong"

    let areContentHeadersImplemented _ =
        unconfigurable, Freya.init true

    let areExpectExtensionsImplemented _ =
        unconfigurable, Freya.init true

    let areHeadersTooLarge =
        decision AreHeadersTooLarge false

    let isFunctionalityImplemented =
        decision IsFunctionalityImplemented true

    let isMethodImplemented _ =
        unconfigurable, Freya.init true

    let isServiceAvailable =
        decision IsServiceAvailable true

    let isSystemOk =
        decision IsSystemOk true

    let isUriTooLong =
        decision IsUriTooLong false

(* Graph *)

[<RequireQualifiedAccess>]
module Graph =

    let operations =
        [ Ref Decisions.AreContentHeadersImplemented            =.        Binary Decisions.areContentHeadersImplemented
          Ref Decisions.AreExpectExtensionsImplemented          =.        Binary Decisions.areExpectExtensionsImplemented
          Ref Decisions.AreHeadersTooLarge                      =.        Binary Decisions.areHeadersTooLarge
          Ref Decisions.IsFunctionalityImplemented              =.        Binary Decisions.isFunctionalityImplemented
          Ref Decisions.IsMethodImplemented                     =.        Binary Decisions.isMethodImplemented
          Ref Decisions.IsServiceAvailable                      =.        Binary Decisions.isServiceAvailable
          Ref Decisions.IsSystemOk                              =.        Binary Decisions.isSystemOk
          Ref Decisions.IsUriTooLong                            =.        Binary Decisions.isUriTooLong

          Start                                                 >.        Ref Decisions.IsServiceAvailable
          Ref Decisions.IsServiceAvailable                      >+        Ref Decisions.IsUriTooLong
          Ref Decisions.IsServiceAvailable                      >-        Ref Common.Operations.ServiceUnavailable
          Ref Decisions.IsUriTooLong                            >+        Ref Common.Operations.UriTooLong
          Ref Decisions.IsUriTooLong                            >-        Ref Decisions.AreHeadersTooLarge
          Ref Decisions.AreHeadersTooLarge                      >+        Ref Common.Operations.HeadersTooLarge
          Ref Decisions.AreHeadersTooLarge                      >-        Ref Decisions.IsMethodImplemented
          Ref Decisions.IsMethodImplemented                     >+        Ref Decisions.AreContentHeadersImplemented
          Ref Decisions.IsMethodImplemented                     >-        Ref Common.Operations.NotImplemented
          Ref Decisions.AreContentHeadersImplemented            >+        Ref Decisions.IsFunctionalityImplemented
          Ref Decisions.AreContentHeadersImplemented            >-        Ref Common.Operations.NotImplemented
          Ref Decisions.IsFunctionalityImplemented              >+        Ref Decisions.AreExpectExtensionsImplemented
          Ref Decisions.IsFunctionalityImplemented              >-        Ref Common.Operations.NotImplemented
          Ref Decisions.AreExpectExtensionsImplemented          >+        Ref Decisions.IsSystemOk
          Ref Decisions.AreExpectExtensionsImplemented          >-        Ref Common.Operations.ExpectationFailed
          Ref Decisions.IsSystemOk                              >-        Ref Common.Operations.InternalServerError ]