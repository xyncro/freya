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

   See [https://github.com/for-GET/http-decision-diagram]. *)

(* Decisions *)

[<RequireQualifiedAccess>]
module Decisions =

    // TODO: Consider ignoring mismatches on a more granular level.
    // We did do this previously in the earlier implementation.

    let [<Literal>] AcceptCharsetMatches = "acceptCharsetMatches"
    let [<Literal>] AcceptEncodingMatches = "acceptEncodingMatches"
    let [<Literal>] AcceptLanguageMatches = "acceptLanguageMatches"
    let [<Literal>] AcceptMatches = "acceptMatches"
    let [<Literal>] HasAccept = "hasAccept"
    let [<Literal>] HasAcceptCharset = "hasAcceptCharset"
    let [<Literal>] HasAcceptEncoding = "hasAcceptEncoding"
    let [<Literal>] HasAcceptLanguage = "hasAcceptLanguage"
    let [<Literal>] IgnoreAcceptMismatches = "ignoreAcceptMismatches"

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

    let ignoreAcceptMismatches =
        decision IgnoreAcceptMismatches true

(* Graph *)

[<RequireQualifiedAccess>]
module Graph =

    let operations =
        [ Ref Decisions.AcceptCharsetMatches               .|=       Binary Decisions.acceptCharsetMatches
          Ref Decisions.AcceptEncodingMatches              .|=       Binary Decisions.acceptEncodingMatches
          Ref Decisions.AcceptLanguageMatches              .|=       Binary Decisions.acceptLanguageMatches
          Ref Decisions.AcceptMatches                      .|=       Binary Decisions.acceptMatches
          Ref Decisions.HasAccept                          .|=       Binary Decisions.hasAccept
          Ref Decisions.HasAcceptCharset                   .|=       Binary Decisions.hasAcceptCharset
          Ref Decisions.HasAcceptEncoding                  .|=       Binary Decisions.hasAcceptEncoding
          Ref Decisions.HasAcceptLanguage                  .|=       Binary Decisions.hasAcceptLanguage
          Ref Decisions.IgnoreAcceptMismatches             .|=       Binary Decisions.ignoreAcceptMismatches
          
          Ref Request.Decisions.IsRequestOk                .+>       Ref Decisions.HasAccept
          Ref Decisions.HasAccept                          .+>       Ref Decisions.AcceptMatches
          Ref Decisions.HasAccept                          .->       Ref Decisions.HasAcceptLanguage
          Ref Decisions.HasAcceptLanguage                  .+>       Ref Decisions.AcceptLanguageMatches
          Ref Decisions.HasAcceptLanguage                  .->       Ref Decisions.HasAcceptCharset
          Ref Decisions.HasAcceptCharset                   .+>       Ref Decisions.AcceptCharsetMatches
          Ref Decisions.HasAcceptCharset                   .->       Ref Decisions.HasAcceptEncoding
          Ref Decisions.HasAcceptEncoding                  .+>       Ref Decisions.AcceptEncodingMatches
          Ref Decisions.AcceptMatches                      .+>       Ref Decisions.HasAcceptLanguage
          Ref Decisions.AcceptMatches                      .->       Ref Decisions.IgnoreAcceptMismatches
          Ref Decisions.AcceptLanguageMatches              .+>       Ref Decisions.HasAcceptCharset
          Ref Decisions.AcceptLanguageMatches              .->       Ref Decisions.IgnoreAcceptMismatches
          Ref Decisions.AcceptCharsetMatches               .+>       Ref Decisions.HasAcceptEncoding
          Ref Decisions.AcceptCharsetMatches               .->       Ref Decisions.IgnoreAcceptMismatches
          Ref Decisions.AcceptEncodingMatches              .->       Ref Decisions.IgnoreAcceptMismatches
          Ref Decisions.IgnoreAcceptMismatches             .->       Ref Common.Operations.NotAcceptable ]
