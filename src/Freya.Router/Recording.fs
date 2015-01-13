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

let [<Literal>] freyaRouterRecordKey =
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
    { Key: string
      Children: FreyaRouterTrieRecord list }

    static member ToJSON (x: FreyaRouterTrieRecord) =
        jobj [
            "key" .= x.Key
            "children" .= x.Children ]

(* Execution *)

and FreyaRouterExecutionRecord =
    { Tries: FreyaRouterExecutionTrieRecord list }

    static member TriesLens =
        (fun x -> x.Tries), (fun t x -> { x with Tries = t })

    static member ToJSON (x: FreyaRouterExecutionRecord) =
        jobj [
            "tries" .= x.Tries ]

and FreyaRouterExecutionTrieRecord =
    { Key: string
      Value: string
      Result: FreyaRouterExecutionResult }

    static member ToJSON (x: FreyaRouterExecutionTrieRecord) =
        jobj [
            "key" .= x.Key
            "value" .= x.Value
            "result" .=
                ((function | Captured -> "captured"
                           | Failed -> "failed"
                           | Matched -> "matched") x.Result) ]

and FreyaRouterExecutionResult =
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

let rec internal routerTrieRecord (trie: CompilationTrie) : FreyaRouterTrieRecord =
    { Key = trie.Key
      Children = trie.Children |> List.map routerTrieRecord }

(* Lenses *)

let freyaRouterRecordPLens =
    freyaRecordDataPLens<FreyaRouterRecord> freyaRouterRecordKey

(* Recording *)

let initializeFreyaRouterRecord =
    updateRecord (setPL freyaRouterRecordPLens freyaRouterRecord)

let internal setFreyaRouterTrieRecord trie =
    updateRecord (setPL (     freyaRouterRecordPLens
                         >?-> FreyaRouterRecord.TrieLens) trie)

let internal addFreyaRouterExecutionRecord key value result =
    updateRecord (modPL (     freyaRouterRecordPLens
                         >?-> FreyaRouterRecord.ExecutionLens
                         >?-> FreyaRouterExecutionRecord.TriesLens)
                        (fun x -> x @ [ { Key = key
                                          Value = value
                                          Result = result } ]))