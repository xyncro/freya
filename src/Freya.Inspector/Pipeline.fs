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
module Freya.Inspector.Pipeline

open Freya.Core
open Freya.Core.Operators
open Freya.Recorder
open Arachne.Http

(* Functions *)

let private initialize =
        FreyaRecorder.Current.initialize
     *> Freya.next

let private record config =
    freya {
        for inspector in config.Inspectors do
            do! inspector.Runtime.Initialize

        return Next }

(* Pipelines *)

let freyaInspector config =
        content
    >?= data config
    >?= initialize
    >?= record config