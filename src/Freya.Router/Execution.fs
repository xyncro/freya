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
open FParsec
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

let private dataLens =
        TraversalState.DataLens
   >--> TraversalData.DataLens

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

let private (|Candidate|_|) traversal =
    match traversal with
    | Traversal (Invariant meth,
                 State (
                     Data ("", data),
                     Position (key, _)) :: _) -> Some (meth, data, key)
    | _ -> None

let private (|Progression|_|) traversal =
    match traversal with
    | Traversal (Invariant _,
                 State (
                     Data (path, _),
                     Position (key, order)) :: _) -> Some (path, key, order)
    | _ -> None

let private (|Parsed|_|) parser path =
    match run parser path with
    | Success (data, _, p) -> Some (data, path.Substring (int p.Index))
    | _ -> None

(* Traversal *)

let private capture key data path =
    (fun states ->
        match states with
        | state :: states ->
            (state
             |> key ^= keyLens
             |> 0 ^= orderLens
             |> (+) data ^%= dataLens
             |> path ^= pathLens) :: state :: states
        | _ -> []) ^%= Traversal.StateLens

let private reject =
    (fun states ->
        match states with
        | state :: states -> (((+) 1) ^%= orderLens) state :: states
        | _ -> []) ^%= Traversal.StateLens

let private abandon =
    (fun states ->
        match states with
        | _ :: state :: states -> (((+) 1) ^%= orderLens) state :: states
        | _ -> []) ^%= Traversal.StateLens

let private tryFindPipe key meth =
        Graph.findNode key
     >> fun node ->
            match node with
            | _, Endpoints endpoints ->
                List.tryPick (fun node ->
                    match node with
                    | Endpoint (Methods m, pipe) when List.exists ((=) meth) m -> Some pipe
                    | Endpoint (All, pipe) -> Some pipe
                    | _ -> None) endpoints
            | _ ->
                None

let private tryFindEdge key order =
        Graph.successors key
     >> fun edges ->
            match edges with
            | Some edges ->
                List.tryPick (fun edge ->
                    match edge with
                    | key', Edge (parser, order') when order = order' -> Some (key', Edge (parser, order))
                    | _ -> None) edges
            | _ ->
                None

let rec private traverse graph traversal =
    match traversal with
    | Candidate (meth, data, key) ->
        match tryFindPipe key meth (graph ^. graphLens) with
        | Some pipe ->
            completionSuccess key *> Freya.init (Matched (data, pipe))
        | _ ->
            completionFailure key *> traverse graph (abandon traversal)
    | Progression (path, key, order) ->
        match tryFindEdge key order (graph ^. graphLens) with
        | Some (key', Edge (parser, _)) ->
            match path with
            | Parsed parser (data, path') ->
                matchSuccess key' *> traverse graph (capture key' data path' traversal)
            | _ ->
                matchFailure key' *> traverse graph (reject traversal)
        | _ ->
            traverse graph (abandon traversal)
    | _ ->
        Freya.init Unmatched

(* Search *)

let private search graph =
        createTraversal <!> (!. Request.meth) <*> (!. Request.path)
    >>= traverse graph

(* Execution *)

let execute graph =
        search graph
    >>= function | Matched (data, pipe) -> (Route.data .?= data) *> pipe
                 | Unmatched -> next