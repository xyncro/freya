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
module internal Freya.Machine.Http.Precondition

open Freya.Machine
open Freya.Machine.Operators

(* Precondition

   Decisions and topology of the section of an HTTP graph classified
   under Precondition, according to the HTTP decision diagram (v4.0.201410),
   defined as part of the for-GET project.

   See [https://github.com/for-GET/http-decision-diagram]. *)

(* Decisions *)

[<RequireQualifiedAccess>]
module Decisions =

    let [<Literal>] HasIfMatch = "hasIfMatch"
    let [<Literal>] HasIfModifiedSince = "hasIfModifiedSince"
    let [<Literal>] HasIfNoneMatch = "hasIfNoneMatch"
    let [<Literal>] HasIfUnmodifiedSince = "hasIfUnmodifiedSince"
    let [<Literal>] IfMatchMatches = "ifMatchMatches"
    let [<Literal>] IfModifiedSinceMatches = "ifModifiedSinceMatches"
    let [<Literal>] IfNoneMatchMatches = "ifNoneMatchMatches"
    let [<Literal>] IfUnmodifiedSinceMatches = "ifUnmodifiedSinceMatches"
    let [<Literal>] IsPreconditionSafe = "isPreconditionSafe"
    let [<Literal>] MissingHasPrecondition = "MissingHasPrecondition"

    let hasIfMatch =
        decision HasIfMatch false

    let hasIfModifiedSince =
        decision HasIfModifiedSince false

    let hasIfNoneMatch =
        decision HasIfNoneMatch false

    let hasIfUnmodifiedSince =
        decision HasIfUnmodifiedSince false

    let ifMatchMatches =
        decision IfMatchMatches false

    let ifModifiedSinceMatches =
        decision IfModifiedSinceMatches false

    let ifNoneMatchMatches =
        decision IfNoneMatchMatches false

    let ifUnmodifiedSinceMatches =
        decision IfUnmodifiedSinceMatches false

    let isPreconditionSafe =
        decision IsPreconditionSafe false

    let missingHasPrecondition =
        decision MissingHasPrecondition false

(* Graph *)

[<RequireQualifiedAccess>]
module Graph =

    let operations =
        [ Ref Decisions.HasIfMatch                         =.        Binary Decisions.hasIfMatch
          Ref Decisions.HasIfModifiedSince                 =.        Binary Decisions.hasIfModifiedSince
          Ref Decisions.HasIfNoneMatch                     =.        Binary Decisions.hasIfNoneMatch
          Ref Decisions.HasIfUnmodifiedSince               =.        Binary Decisions.hasIfUnmodifiedSince
          Ref Decisions.IfMatchMatches                     =.        Binary Decisions.ifMatchMatches
          Ref Decisions.IfModifiedSinceMatches             =.        Binary Decisions.ifModifiedSinceMatches
          Ref Decisions.IfNoneMatchMatches                 =.        Binary Decisions.ifNoneMatchMatches
          Ref Decisions.IfUnmodifiedSinceMatches           =.        Binary Decisions.ifUnmodifiedSinceMatches
          Ref Decisions.IsPreconditionSafe                 =.        Binary Decisions.isPreconditionSafe
          Ref Decisions.MissingHasPrecondition             =.        Binary Decisions.missingHasPrecondition

          Ref Retrieve.Decisions.Missing                   >+        Ref Decisions.MissingHasPrecondition
          Ref Retrieve.Decisions.Missing                   >-        Ref Decisions.HasIfMatch
          Ref Decisions.MissingHasPrecondition             >+        Ref Common.Operations.PreconditionFailed
          Ref Decisions.MissingHasPrecondition             >-        Ref Retrieve.Decisions.Moved
          Ref Decisions.HasIfMatch                         >+        Ref Decisions.IfMatchMatches
          Ref Decisions.HasIfMatch                         >-        Ref Decisions.HasIfUnmodifiedSince
          Ref Decisions.HasIfUnmodifiedSince               >+        Ref Decisions.IfUnmodifiedSinceMatches
          Ref Decisions.HasIfUnmodifiedSince               >-        Ref Decisions.HasIfNoneMatch
          Ref Decisions.IfUnmodifiedSinceMatches           >+        Ref Decisions.HasIfNoneMatch
          Ref Decisions.IfUnmodifiedSinceMatches           >-        Ref Common.Operations.PreconditionFailed
          Ref Decisions.IfMatchMatches                     >+        Ref Decisions.HasIfNoneMatch
          Ref Decisions.IfMatchMatches                     >-        Ref Common.Operations.PreconditionFailed
          Ref Decisions.HasIfNoneMatch                     >+        Ref Decisions.IfNoneMatchMatches
          Ref Decisions.HasIfNoneMatch                     >-        Ref Decisions.HasIfModifiedSince
          Ref Decisions.HasIfModifiedSince                 >+        Ref Decisions.IfModifiedSinceMatches
          Ref Decisions.IfModifiedSinceMatches             >-        Ref Decisions.IsPreconditionSafe
          Ref Decisions.IfNoneMatchMatches                 >-        Ref Decisions.IsPreconditionSafe
          Ref Decisions.IsPreconditionSafe                 >+        Ref Common.Operations.NotModified
          Ref Decisions.IsPreconditionSafe                 >-        Ref Common.Operations.PreconditionFailed ]

