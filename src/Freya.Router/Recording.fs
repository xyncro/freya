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
//----------------------------------------------------------------------------

[<AutoOpen>]
module Freya.Router.Recording

open Aether
open Fleece
open Fleece.Operators
open Freya.Core
open Freya.Recorder

(* Keys *)

let [<Literal>] internal routerKey =
    "router"

(* Types *)

type FreyaRouterRecord =
    { Execution: string list }

    static member ToJSON (x: FreyaRouterRecord) =
        jobj [
            "execution" .= x.Execution ]

(* Contructors *)

let private freyaRouterRecord =
    { Execution = List.empty }

(* Lenses *)

let internal freyaRouterRecordPLens =
    recordDataPLens<FreyaRouterRecord> routerKey

(* Functions *)

let internal initFreyaRouterR () =
    modR (setPL freyaRouterRecordPLens freyaRouterRecord)