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
module internal Freya.Machine.Http.Accept

open Freya.Core
open Freya.Machine
open Freya.Machine.Operators

(* Accept

   Decisions and topology of the section of an HTTP graph classified
   under Accept, according to the HTTP decision diagram (v4.0.201410),
   defined as part of the for-GET project.

   See [https://github.com/for-GET/http-decision-diagram].

   Delta/Notes:

   * Additional decisions added to give a granular configuration of which
     Accept-* header mismatches can be ignored, as opposed to the diagram
     defined single decision.
   * Additional final decision isAcceptOk to give a custom opportunity to
     return a NotAcceptable response for reasons outside the scope of the
     common headers (custom Accept-* semantic headers may be valid).
   * Removal of node ignoreAcceptBlockMismatches
   * Addition of nodes:
     - ignoreAcceptCharsetMismatch
     - ignoreAcceptEncodingMismatch
     - ignoreAcceptLanguageMismatch
     - ignoreAcceptMismatch
   * Addition of node isAcceptOk. *)

(* Decisions *)

[<RequireQualifiedAccess>]
module Decisions =

    let [<Literal>] AcceptCharsetMatches = "acceptCharsetMatches"
    let [<Literal>] AcceptEncodingMatches = "acceptEncodingMatches"
    let [<Literal>] AcceptLanguageMatches = "acceptLanguageMatches"
    let [<Literal>] AcceptMatches = "acceptMatches"
    let [<Literal>] HasAccept = "hasAccept"
    let [<Literal>] HasAcceptCharset = "hasAcceptCharset"
    let [<Literal>] HasAcceptEncoding = "hasAcceptEncoding"
    let [<Literal>] HasAcceptLanguage = "hasAcceptLanguage"
    let [<Literal>] IgnoreAcceptCharsetMismatch = "ignoreAcceptCharsetMismatch"
    let [<Literal>] IgnoreAcceptEncodingMismatch = "ignoreAcceptEncodingMismatch"
    let [<Literal>] IgnoreAcceptLanguageMismatch = "ignoreAcceptLanguageMismatch"
    let [<Literal>] IgnoreAcceptMismatch = "ignoreAcceptMismatch"
    let [<Literal>] IsAcceptOk = "isAcceptOk"

    let acceptCharsetMatches _ =
        unconfigurable, Freya.init true

    let acceptEncodingMatches _ =
        unconfigurable, Freya.init true

    let acceptLanguageMatches _ =
        unconfigurable, Freya.init true

    let acceptMatches _ =
        unconfigurable, Freya.init true

    let hasAccept _ =
        unconfigurable, Freya.init true

    let hasAcceptCharset _ =
        unconfigurable, Freya.init true

    let hasAcceptEncoding _ =
        unconfigurable, Freya.init true

    let hasAcceptLanguage _ =
        unconfigurable, Freya.init true

    let ignoreAcceptCharsetMismatch =
        decision IgnoreAcceptCharsetMismatch true

    let ignoreAcceptEncodingMismatch =
        decision IgnoreAcceptEncodingMismatch true

    let ignoreAcceptLanguageMismatch =
        decision IgnoreAcceptLanguageMismatch true

    let ignoreAcceptMismatch =
        decision IgnoreAcceptMismatch true

    let isAcceptOk =
        decision IsAcceptOk true

(* Graph *)

[<RequireQualifiedAccess>]
module Graph =

    let operations =
        [ Ref Decisions.AcceptCharsetMatches                    =.        Binary Decisions.acceptCharsetMatches
          Ref Decisions.AcceptEncodingMatches                   =.        Binary Decisions.acceptEncodingMatches
          Ref Decisions.AcceptLanguageMatches                   =.        Binary Decisions.acceptLanguageMatches
          Ref Decisions.AcceptMatches                           =.        Binary Decisions.acceptMatches
          Ref Decisions.HasAccept                               =.        Binary Decisions.hasAccept
          Ref Decisions.HasAcceptCharset                        =.        Binary Decisions.hasAcceptCharset
          Ref Decisions.HasAcceptEncoding                       =.        Binary Decisions.hasAcceptEncoding
          Ref Decisions.HasAcceptLanguage                       =.        Binary Decisions.hasAcceptLanguage
          Ref Decisions.IgnoreAcceptCharsetMismatch             =.        Binary Decisions.ignoreAcceptCharsetMismatch
          Ref Decisions.IgnoreAcceptEncodingMismatch            =.        Binary Decisions.ignoreAcceptEncodingMismatch
          Ref Decisions.IgnoreAcceptLanguageMismatch            =.        Binary Decisions.ignoreAcceptLanguageMismatch
          Ref Decisions.IgnoreAcceptMismatch                    =.        Binary Decisions.ignoreAcceptMismatch
          Ref Decisions.IsAcceptOk                              =.        Binary Decisions.isAcceptOk

          Ref Request.Decisions.IsRequestOk                     >+        Ref Decisions.HasAccept
          Ref Decisions.HasAccept                               >+        Ref Decisions.AcceptMatches
          Ref Decisions.HasAccept                               >-        Ref Decisions.HasAcceptLanguage
          Ref Decisions.AcceptMatches                           >+        Ref Decisions.HasAcceptLanguage
          Ref Decisions.AcceptMatches                           >-        Ref Decisions.IgnoreAcceptMismatch
          Ref Decisions.IgnoreAcceptMismatch                    >+        Ref Decisions.HasAcceptLanguage
          Ref Decisions.IgnoreAcceptMismatch                    >-        Ref Common.Operations.NotAcceptable
          Ref Decisions.HasAcceptLanguage                       >+        Ref Decisions.AcceptLanguageMatches
          Ref Decisions.HasAcceptLanguage                       >-        Ref Decisions.HasAcceptCharset
          Ref Decisions.AcceptLanguageMatches                   >+        Ref Decisions.HasAcceptCharset
          Ref Decisions.AcceptLanguageMatches                   >-        Ref Decisions.IgnoreAcceptLanguageMismatch
          Ref Decisions.IgnoreAcceptLanguageMismatch            >+        Ref Decisions.HasAcceptCharset
          Ref Decisions.IgnoreAcceptLanguageMismatch            >-        Ref Common.Operations.NotAcceptable
          Ref Decisions.HasAcceptCharset                        >+        Ref Decisions.AcceptCharsetMatches
          Ref Decisions.HasAcceptCharset                        >-        Ref Decisions.HasAcceptEncoding
          Ref Decisions.AcceptCharsetMatches                    >+        Ref Decisions.HasAcceptEncoding
          Ref Decisions.AcceptCharsetMatches                    >-        Ref Decisions.IgnoreAcceptCharsetMismatch
          Ref Decisions.IgnoreAcceptCharsetMismatch             >+        Ref Decisions.HasAcceptEncoding
          Ref Decisions.IgnoreAcceptCharsetMismatch             >-        Ref Common.Operations.NotAcceptable
          Ref Decisions.HasAcceptEncoding                       >+        Ref Decisions.AcceptEncodingMatches
          Ref Decisions.HasAcceptEncoding                       >-        Ref Decisions.IsAcceptOk
          Ref Decisions.AcceptEncodingMatches                   >+        Ref Decisions.IsAcceptOk
          Ref Decisions.AcceptEncodingMatches                   >-        Ref Decisions.IgnoreAcceptEncodingMismatch
          Ref Decisions.IgnoreAcceptEncodingMismatch            >+        Ref Decisions.IsAcceptOk
          Ref Decisions.IgnoreAcceptEncodingMismatch            >-        Ref Common.Operations.NotAcceptable
          Ref Decisions.IsAcceptOk                              >-        Ref Common.Operations.NotAcceptable ]
