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
module Freya.Machine.Inspector.Inspector

open Aether.Operators
open Chiron
open Freya.Core
open Freya.Inspector
open Freya.Machine
open Freya.Recorder

(* Keys *)

let private key =
    "machine"

(* Lenses *)

let private record_ =
    Record.record_ key

(* Runtime *)

let private record =
    { Recording.Execution =
        { Nodes = List.empty }
      Recording.Graph =
        { Nodes = List.empty
          Edges = List.empty } }

let private initialize =
    FreyaRecorder.Current.map (Some record ^= record_)

let private runtime =
    { Initialize = initialize }

(* Inspection *)

let private extract =
    flip (^.) record_ >> Option.map (FreyaMachineInspection.OfRecord >> Json.serialize)

let private inspection =
    { Extract = extract }

(* Inspector *)

let freyaMachineInspector =
    { Key = key
      Runtime = runtime
      Inspection = inspection }
