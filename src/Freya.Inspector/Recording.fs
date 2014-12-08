[<AutoOpen>]
module Freya.Inspector.Recording

open Aether
open Fleece
open Fleece.Operators
open Freya.Recorder
open Freya.Types.Http

(* Keys *)

let [<Literal>] private requestRecordKey =
    "request"

(* Types *)

type FreyaRequestRecord =
    { Method: Method
      Path: string }

(* Constructors *)

let private freyaRequestRecord meth path =
    { Method = meth
      Path = path }

(* Lenses *)

let internal freyaRequestRecordPLens =
    recordDataPLens<FreyaRequestRecord> requestRecordKey

(* Serialization *)

type FreyaRequestRecord with

    static member ToJSON (x: FreyaRequestRecord) =
        jobj [
            "method" .= Method.Format (x.Method)
            "path" .= x.Path ]

(* Functions *)

let internal initFreyaRequestR meth path =
    modR (setPL freyaRequestRecordPLens (freyaRequestRecord meth path))