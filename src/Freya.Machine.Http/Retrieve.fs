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
module internal Freya.Machine.Http.Retrieve

open Freya.Machine
open Freya.Machine.Operators

(* Retrieve

   Decisions and topology of the section of an HTTP graph classified
   under Retrieve, according to the HTTP decision diagram (v4.0.201410),
   defined as part of the for-GET project.

   See [https://github.com/for-GET/http-decision-diagram]. *)

(* Decisions *)

[<RequireQualifiedAccess>]
module Decisions =

    let [<Literal>] GonePermanently = "gonePermanently"
    let [<Literal>] Missing = "missing"
    let [<Literal>] Moved = "moved"
    let [<Literal>] MovedPermanently = "movedPermanently"
    let [<Literal>] MovedTemporarily = "movedTemporarily"

    let gonePermanently =
        decision GonePermanently false

    let missing =
        decision Missing false

    let moved =
        decision Moved false

    let movedPermanently =
        decision MovedPermanently false

    let movedTemporarily =
        decision MovedTemporarily false

(* Graph *)

[<RequireQualifiedAccess>]
module Graph =

    let operations =
        [ Ref Decisions.GonePermanently                    .|=       Binary Decisions.gonePermanently
          Ref Decisions.Missing                            .|=       Binary Decisions.missing
          Ref Decisions.Moved                              .|=       Binary Decisions.moved
          Ref Decisions.MovedPermanently                   .|=       Binary Decisions.movedPermanently
          Ref Decisions.MovedTemporarily                   .|=       Binary Decisions.movedTemporarily

          Ref Accept.Decisions.HasAcceptEncoding           .->       Ref Decisions.Missing
          Ref Accept.Decisions.AcceptEncodingMatches       .+>       Ref Decisions.Missing
          Ref Accept.Decisions.IgnoreAcceptMismatches      .+>       Ref Decisions.Missing
          Ref Decisions.Moved                              .+>       Ref Decisions.MovedPermanently
          Ref Decisions.Moved                              .->       Finish
          Ref Decisions.MovedPermanently                   .+>       Ref Common.Operations.PermanentRedirect
          Ref Decisions.MovedPermanently                   .->       Ref Decisions.MovedTemporarily
          Ref Decisions.MovedTemporarily                   .+>       Ref Common.Operations.TemporaryRedirect
          Ref Decisions.MovedTemporarily                   .->       Ref Decisions.GonePermanently
          Ref Decisions.GonePermanently                    .+>       Ref Common.Operations.Gone
          Ref Decisions.GonePermanently                    .->       Finish ]
