[<AutoOpen>]
module Freya.Recorder.Types

open System
open Aether
open Aether.Operators
open Freya.Core

(* Low Level Store *)

type FreyaRecorderRecord =
    { Id: Guid
      Timestamp: DateTime
      Data: Map<string, obj> }

(* Lenses *)

let private dataLens =
    (fun x -> x.Data), (fun d x -> { x with Data = d })

let recordDataPLens<'a> key =
    dataLens >-?> mapPLens key <?-> boxIso<'a>