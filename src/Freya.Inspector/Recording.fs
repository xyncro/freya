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
//
//----------------------------------------------------------------------------

[<AutoOpen>]
module Freya.Inspector.Recording

open Aether
open Aether.Operators
open Chiron
open Chiron.Operators
open Freya.Recorder
open Arachne.Http

(* Keys *)

let [<Literal>] internal recordKey =
    "request"

(* Types *)

type FreyaRequestRecord =
    { Method: Method
      Path: string }

    static member ToJson (x: FreyaRequestRecord) =
            Json.write "method" (x.Method.ToString ())
         *> Json.write "path" x.Path

(* Constructors *)

let private freyaRequestRecord meth path =
    { Method = meth
      Path = path }

(* Lenses *)

let internal requestRecordPLens =
    FreyaRecorderRecord.KeyPLens<FreyaRequestRecord> recordKey

(* Initialization *)

let internal initializeRecord (meth, path) =
    FreyaRecorder.Current.map (freyaRequestRecord meth path ^?= requestRecordPLens)