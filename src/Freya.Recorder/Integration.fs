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
module Freya.Recorder.Integration

open System
open Freya.Core
open Freya.Core.Operators

(* Lenses *)

let private requestIdPLens =
    environmentKeyPLens<Guid> requestIdKey

(* Execution *)

let initializeRecord =
    setPLM requestIdPLens =<< (Freya.fromAsync initialize =<< Freya.init ())

let updateRecord f =
    Option.iter (fun id -> update id f) <!> getPLM requestIdPLens

(* Inspection *)

let listRecords =
    Freya.fromAsync list =<< Freya.init ()

let getRecord id =
    Freya.fromAsync read =<< Freya.init id