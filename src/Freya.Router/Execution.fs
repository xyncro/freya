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
module internal Freya.Router.Execution

open Aether
open Aether.Operators
open Freya.Core
open Freya.Core.Operators
open Freya.Pipeline
open Freya.Types.Http
open Freya.Types.Uri.Template
open Hekate

(* Types *)

type ExecutionResult =
    | Matched of UriTemplateData * FreyaPipeline
    | Unmatched

type Traversal =
    | Traversal of TraversalInvariant * TraversalState list

    static member StateLens : Lens<Traversal, TraversalState list> =
        (fun (Traversal (_, s)) -> s),
        (fun s (Traversal (i, _)) -> Traversal (i, s))

and TraversalInvariant =
    | Invariant of Method

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

    static member OrderLens : Lens<TraversalPosition, int> =
        (fun (Position (_, i)) -> i),
        (fun i (Position (k, _)) -> Position (k, i))

(* Constructors *)

let private createTraversal meth path =
    Traversal (
        Invariant (meth), 
        State (
            Data (path, UriTemplateData Map.empty),
            Position (Root, 0)) :: [])

(* Lenses *)

let private pathLens =
         TraversalState.DataLens
    >--> TraversalData.PathLens

let private keyLens =
         TraversalState.PositionLens
    >--> TraversalPosition.KeyLens

let private orderLens =
         TraversalState.PositionLens
    >--> TraversalPosition.OrderLens

(* Patterns *)

let private (|Candidate|_|) =
    function | Traversal (Invariant meth,
                          State (
                              Data ("", data),
                              Position (key, _)) :: _) -> Some (meth, data, key)
             | _ -> None

let private (|Progression|_|) =
    function | Traversal (Invariant _,
                          State (
                              Data (path, _),
                              Position (key, order)) :: _) -> Some (path, key, order)
             | _ -> None

(* Traversal *)

let private capture key path =
    (function | state :: states ->
                    (state
                     |> key ^= keyLens
                     |> 0 ^= orderLens
                     |> path ^= pathLens) :: state :: states
              | _ -> []) ^%= Traversal.StateLens

let private reject =
    (function | state :: states -> (((+) 1) ^%= orderLens) state :: states
              | _ -> []) ^%= Traversal.StateLens

let private abandon =
    (function | _ :: state :: states -> (((+) 1) ^%= orderLens) state :: states
              | _ -> []) ^%= Traversal.StateLens

let rec private traverse graph traversal =
    freya {
        match traversal with
        | Candidate (meth, data, key) ->
            let pipe =
                (fun graph ->
                    match Graph.findNode key graph with
                    | _, Endpoints endpoints ->
                        List.tryPick (fun node ->
                            match node with
                            | Endpoint (Methods m, pipe) when List.exists ((=) meth) m -> Some pipe
                            | Endpoint (All, pipe) -> Some pipe
                            | _ -> None) endpoints
                    | _ ->
                        None) (graph ^. graphLens)

            match pipe with
            | Some pipe -> return Matched (data, pipe)
            | _ -> return! traverse graph (abandon traversal)
        | Progression (path, key, order) ->
            let edge =
                (fun graph ->
                    match Graph.successors key graph with
                    | Some edges ->
                        List.tryPick (fun edge ->
                            match edge with
                            | key', Edge (part, order') when order = order' -> Some (key', Edge (part, order))
                            | _ -> None) edges
                    | _ ->
                        None) (graph ^. graphLens)

            match edge with
            | Some (key', Edge (part, _)) ->
                match part.Match path with
                | Some data, Some path' -> return! traverse graph (capture key' path' traversal)
                | _, Some path' -> return! traverse graph (capture key' path' traversal)
                | _ -> return! traverse graph (reject traversal)
            | _ ->
                return! traverse graph (abandon traversal)
        | _ ->
            return Unmatched }

(* Search *)

let private search graph =
        createTraversal <!> (!. Request.meth) <*> (!. Request.path)
    >>= traverse graph

(* Execution *)

let execute graph =
        search graph
    >>= function | Matched (data, pipe) -> (Route.data .?= data) *> pipe
                 | Unmatched -> next