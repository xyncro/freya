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
module Freya.Router.Execution

open Aether
open Aether.Operators
open Freya.Types.Http
open Freya.Types.Uri.Template

(* Types *)

type Traversal =
    | Traversal of TraversalInvariant * TraversalState list

    static member InvariantLens : Lens<Traversal, TraversalInvariant> =
        (fun (Traversal (i, _)) -> i),
        (fun i (Traversal (_, s)) -> Traversal (i, s))

    static member StateLens : Lens<Traversal, TraversalState list> =
        (fun (Traversal (_, s)) -> s),
        (fun s (Traversal (i, _)) -> Traversal (i, s))

and TraversalInvariant =
    | Invariant of Method

    static member MethodLens : Lens<TraversalInvariant, Method> =
        (fun (Invariant (m)) -> m),
        (fun m (Invariant (_)) -> Invariant (m))

and TraversalState =
    | State of TraversalData * TraversalPosition

    static member DataLens : Lens<TraversalState, TraversalData> =
        (fun (State (d, _)) -> d),
        (fun d (State (_, p)) -> State (d, p))

    static member PositionLens : Lens<TraversalState, TraversalPosition> =
        (fun (State (_, p)) -> p),
        (fun p (State (d, _)) -> State (d, p))

and TraversalData =
    | Data of string * UriTemplateData

    static member PathLens : Lens<TraversalData, string> =
        (fun (Data (p, _)) -> p),
        (fun p (Data (_, d)) -> Data (p, d))

    static member DataLens : Lens<TraversalData, UriTemplateData> =
        (fun (Data (_, d)) -> d),
        (fun d (Data (p, _)) -> Data (p, d))

and TraversalPosition =
    | Position of CompilationKey * int

    static member KeyLens : Lens<TraversalPosition, CompilationKey> =
        (fun (Position (k, _)) -> k),
        (fun k (Position (_, i)) -> Position (k, i))

    static member DataLens : Lens<TraversalPosition, int> =
        (fun (Position (_, i)) -> i),
        (fun i (Position (k, _)) -> Position (k, i))

(* Defaults *)

let defaultTraversal meth path =
    Traversal (
        Invariant (meth), 
        State (
            Data (path, UriTemplateData Map.empty),
            Position (Root, 0)) :: [])