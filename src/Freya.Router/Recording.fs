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
open Freya.Recorder

(* Keys *)

let [<Literal>] internal routerKey =
    "router"

(* Types *)

type FreyaRouterRecord =
    { Trie: FreyaRouterTrieRecord
      Execution: FreyaRouterExecutionRecord }

    static member ToJSON (x: FreyaRouterRecord) =
        jobj [
            "trie" .= x.Trie
            "execution" .= x.Execution ]

(* Trie *)

and FreyaRouterTrieRecord =
    { Id: string }

    static member ToJSON (x: FreyaRouterTrieRecord) =
        jobj [
            "id" .= x.Id ]

(* Execution *)

and FreyaRouterExecutionRecord =
    { Tries: string list }

    static member ToJSON (x: FreyaRouterExecutionRecord) =
        jobj [
            "tries" .= x.Tries ]

(* Contructors *)

let private freyaRouterRecord =
    { Trie =
        { Id = "" }
      Execution =
        { Tries = List.empty } }

(* Lenses *)

let internal freyaRouterRecordPLens =
    recordDataPLens<FreyaRouterRecord> routerKey

(* Functions *)

let internal initializeFreyaRouterRecord =
    updateRecord (setPL freyaRouterRecordPLens freyaRouterRecord)