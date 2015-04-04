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

[<AutoOpen>]
module Freya.Router.Recording

open Aether
open Aether.Operators
open Chiron
open Freya.Recorder

(* Keys *)

let [<Literal>] freyaRouterRecordKey =
    "router"

(* Types *)

type FreyaRouterRecord =
    { Lines: string list }

    static member LinesLens =
        (fun x -> x.Lines), (fun l x -> { x with Lines = l })

    static member ToJson (x: FreyaRouterRecord) =
        Json.write "lines" x.Lines

(* Defaults *)

let private defaultFreyaRouterRecord =
    { Lines = List.empty }

(* Lenses *)

let freyaRouterRecordPLens =
    freyaRecordDataPLens<FreyaRouterRecord> freyaRouterRecordKey

let private linesPLens =
         freyaRouterRecordPLens
    >?-> FreyaRouterRecord.LinesLens

(* Recording *)

let initializeFreyaRouterRecord =
    updateRecord (Lens.setPartial freyaRouterRecordPLens defaultFreyaRouterRecord)

let internal addFreyaRouterLineRecord line =
    updateRecord (Lens.mapPartial linesPLens (fun lines -> line :: lines))
