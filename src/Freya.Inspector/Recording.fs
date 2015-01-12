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
module Freya.Inspector.Recording

open Aether
open Fleece
open Fleece.Operators
open Freya.Recorder
open Freya.Types.Http

(* Keys *)

let [<Literal>] internal requestRecordKey =
    "request"

(* Types *)

type RequestRecord =
    { Method: Method
      Path: string }

    static member ToJSON (x: RequestRecord) =
        jobj [
            "method" .= Method.Format (x.Method)
            "path" .= x.Path ]

(* Constructors *)

let private requestRecord meth path =
    { Method = meth
      Path = path }

(* Lenses *)

let internal requestRecordPLens =
    recordDataPLens<RequestRecord> requestRecordKey

(* Functions *)

let internal initializeRequestRecord meth path =
    updateRecord (setPL requestRecordPLens (requestRecord meth path))