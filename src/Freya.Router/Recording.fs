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