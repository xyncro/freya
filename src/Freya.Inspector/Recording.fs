[<AutoOpen>]
module Freya.Inspector.Recording

open Aether
open Fleece
open Fleece.Operators
open Freya.Recorder
open Freya.Types.Http

(* Keys *)

let [<Literal>] internal requestKey =
    "request"

(* Types *)

type FreyaRequestRecord =
    { Method: Method
      Path: string }

    static member ToJSON (x: FreyaRequestRecord) =
        jobj [
            "method" .= Method.Format (x.Method)
            "path" .= x.Path ]

(* Constructors *)

let private freyaRequestRecord meth path =
    { Method = meth
      Path = path }

(* Lenses *)

let internal freyaRequestRecordPLens =
    recordDataPLens<FreyaRequestRecord> requestKey

(* Functions *)

let internal initializeFreyaRequestRecord meth path =
    updateRecord (setPL freyaRequestRecordPLens (freyaRequestRecord meth path))