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

let [<Literal>] routerRecordKey =
    "router"

(* Types *)

type RouterRecord =
    { Execution: RouterExecutionRecord
      Trie: RouterTrieRecord }

    static member ExecutionLens =
        (fun x -> x.Execution), (fun e x -> { x with Execution = e })

    static member TrieLens =
        (fun x -> x.Trie), (fun t x -> { x with Trie = t })

    static member ToJSON (x: RouterRecord) =
        jobj [
            "execution" .= x.Execution
            "trie" .= x.Trie ]

(* Trie *)

and RouterTrieRecord =
    { Key: string
      Children: RouterTrieRecord list }

    static member ToJSON (x: RouterTrieRecord) =
        jobj [
            "key" .= x.Key
            "children" .= x.Children ]

(* Execution *)

and RouterExecutionRecord =
    { Tries: RouterExecutionTrieRecord list }

    static member TriesLens =
        (fun x -> x.Tries), (fun t x -> { x with Tries = t })

    static member ToJSON (x: RouterExecutionRecord) =
        jobj [
            "tries" .= x.Tries ]

and RouterExecutionTrieRecord =
    { Key: string
      Value: string
      Result: RouterExecutionResult }

    static member ToJSON (x: RouterExecutionTrieRecord) =
        jobj [
            "key" .= x.Key
            "value" .= x.Value
            "result" .=
                ((function | Captured -> "captured"
                           | Failed -> "failed"
                           | Matched -> "matched") x.Result) ]

and RouterExecutionResult =
    | Captured
    | Failed
    | Matched

(* Constructors *)

let private freyaRouterRecord =
    { Trie =
        { Key = ""
          Children = List.empty }
      Execution =
        { Tries = List.empty } }

let rec internal routerTrieRecord (trie: RouterTrie) : RouterTrieRecord =
    { Key = trie.Key
      Children = trie.Children |> List.map routerTrieRecord }

(* Lenses *)

let routerRecordPLens =
    recordDataPLens<RouterRecord> routerRecordKey

(* Recording *)

let initializeRouterRecord =
    updateRecord (setPL routerRecordPLens freyaRouterRecord)

let internal setRouterTrieRecord trie =
    updateRecord (setPL (     routerRecordPLens
                         >?-> RouterRecord.TrieLens) trie)

let internal addRouterExecutionRecord key value result =
    updateRecord (modPL (     routerRecordPLens
                         >?-> RouterRecord.ExecutionLens
                         >?-> RouterExecutionRecord.TriesLens)
                        (fun x -> x @ [ { Key = key
                                          Value = value
                                          Result = result } ]))