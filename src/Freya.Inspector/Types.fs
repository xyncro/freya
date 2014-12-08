[<AutoOpen>]
module Freya.Inspector.Types

open System
open System.Json
open Fleece
open Fleece.Operators
open Freya.Core
open Freya.Recorder

(* Types *)

type FreyaInspectorConfiguration =
    { Inspectors: FreyaInspector list }

and FreyaInspector =
    { Id: string
      Runtime: FreyaInspectorRuntime
      Inspection: FreyaInspectorInspection }

and FreyaInspectorRuntime =
    { Initialize: Freya<unit> }

and FreyaInspectorInspection =
    { Data: FreyaRecorderRecord -> JsonValue option }