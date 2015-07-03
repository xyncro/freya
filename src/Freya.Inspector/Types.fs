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
module Freya.Inspector.Types

open System
open Chiron
open Chiron.Operators
open Freya.Core
open Freya.Recorder

(* Types *)

type FreyaInspectorConfiguration =
    { Inspectors: FreyaInspector list }

and FreyaInspector =
    { Key: string
      Runtime: FreyaInspectorRuntime
      Inspection: FreyaInspectorInspection }

and FreyaInspectorRuntime =
    { Initialize: Freya<unit> }

and FreyaInspectorInspection =
    { Extract: FreyaRecorderRecord -> Json option }

(* Record Models *)

type FreyaRecorderRecordHeader =
    { Id: Guid
      Timestamp: DateTime }

    static member ToJson (x: FreyaRecorderRecordHeader) =
            Json.write "id" x.Id
         *> Json.write "timestamp" x.Timestamp

type FreyaRecorderRecordDetail =
    { Id: Guid
      Timestamp: DateTime
      Extensions: string list }

    static member ToJson (x: FreyaRecorderRecordDetail) =
            Json.write "id" x.Id
         *> Json.write "timestamp" x.Timestamp
         *> Json.write "extensions" x.Extensions