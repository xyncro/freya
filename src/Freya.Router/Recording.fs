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
open Aether.Operators
open Fleece
open Fleece.Operators
open Freya.Recorder

(* Keys *)

let [<Literal>] internal routerKey =
    "router"

(* Types *)

type FreyaRouterRecord =
    { Execution: FreyaRouterExecutionRecord
      Trie: FreyaRouterTrieRecord }

    static member ExecutionLens =
        (fun x -> x.Execution), (fun e x -> { x with Execution = e })

    static member TrieLens =
        (fun x -> x.Trie), (fun t x -> { x with Trie = t })

    static member ToJSON (x: FreyaRouterRecord) =
        jobj [
            "execution" .= x.Execution
            "trie" .= x.Trie ]

(* Trie *)

and FreyaRouterTrieRecord =
    { Id: string
      Children: FreyaRouterTrieRecord list }

    static member ToJSON (x: FreyaRouterTrieRecord) =
        jobj [
            "id" .= x.Id
            "children" .= x.Children ]

(* Execution *)

and FreyaRouterExecutionRecord =
    { Tries: string list }

    static member ToJSON (x: FreyaRouterExecutionRecord) =
        jobj [
            "tries" .= x.Tries ]

(* Constructors *)

let private freyaRouterRecord =
    { Trie =
        { Id = ""
          Children = List.empty }
      Execution =
        { Tries = List.empty } }

(* Lenses *)

let internal freyaRouterRecordPLens =
    recordDataPLens<FreyaRouterRecord> routerKey

(* Functions *)

let internal trieRecord trie =
    ()

(* Recording *)

let internal initializeRecord =
    updateRecord (setPL freyaRouterRecordPLens freyaRouterRecord)

let internal setTrieRecord trie =
    updateRecord (setPL (     freyaRouterRecordPLens
                         >?-> FreyaRouterRecord.TrieLens) trie)