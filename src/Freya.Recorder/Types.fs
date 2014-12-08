[<AutoOpen>]
module Freya.Recorder.Types

open System
open Aether
open Aether.Operators
open Fleece
open Fleece.Operators
open Freya.Core

(* Types *)

type FreyaRecorderRecord =
    { Id: Guid
      Timestamp: DateTime
      Data: Map<string, obj> }

    static member ToJSON (x: FreyaRecorderRecord) =
        jobj [
            "id" .= x.Id
            "timestamp" .= x.Timestamp
            "inspections" .= (x.Data |> Map.toList |> List.map fst) ]

(* Lenses *)

let private dataLens =
    (fun x -> x.Data), (fun d x -> { x with Data = d })

let recordDataPLens<'a> key =
    dataLens >-?> mapPLens key <?-> boxIso<'a>