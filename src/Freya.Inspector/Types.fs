[<AutoOpen>]
module Freya.Inspector.Types

open System
open Fleece
open Fleece.Operators
open Freya.Core

(* Types *)

type FreyaInspectorConfiguration =
    { Inspectors: FreyaInspector list }

and FreyaInspector =
    { Initialize: Freya<unit> }

type FreyaInspectorRecord =
    { Id: Guid
      Timestamp: DateTime
      Inspections: string list }

(* Serialization *)

type FreyaInspectorRecord with

    static member ToJSON (x: FreyaInspectorRecord) =
        jobj [
            "id" .= x.Id
            "timstamp" .= x.Timestamp
            "inspections" .= x.Inspections ]