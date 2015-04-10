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

(* Types

   Types representing the potential outcome of a router execution,
   as well as the intermediate state tracked throughout a traversal
   of the compiled routing graph.

   We keep an effective stack of states, particularly while capturing
   new data to enable a backtracking depth first search over a tree
   built from the complete set of routes registered with the router
   initially. *)

type private ExecutionResult =
    | Matched of UriTemplateData * FreyaPipeline
    | Unmatched

type private Traversal =
    | Traversal of TraversalInvariant * TraversalState list

    static member StateLens : Lens<Traversal, TraversalState list> =
        (fun (Traversal (_, s)) -> s),
        (fun s (Traversal (i, _)) -> Traversal (i, s))

and private TraversalInvariant =
    | Invariant of Method

and private TraversalState =
    | State of TraversalData * TraversalPosition

    static member DataLens : Lens<TraversalState, TraversalData> =
        (fun (State (d, _)) -> d), (fun d (State (_, p)) -> State (d, p))

    static member PositionLens : Lens<TraversalState, TraversalPosition> =
        (fun (State (_, p)) -> p), (fun p (State (d, _)) -> State (d, p))

and private TraversalData =
    | Data of string * UriTemplateData

    static member PathLens : Lens<TraversalData, string> =
        (fun (Data (p, _)) -> p), (fun p (Data (_, d)) -> Data (p, d))

    static member DataLens : Lens<TraversalData, UriTemplateData> =
        (fun (Data (_, d)) -> d), (fun d (Data (p, _)) -> Data (p, d))

and private TraversalPosition =
    | Position of CompilationKey * int

    static member KeyLens : Lens<TraversalPosition, CompilationKey> =
        (fun (Position (k, _)) -> k), (fun k (Position (_, i)) -> Position (k, i))

    static member OrderLens : Lens<TraversalPosition, int> =
        (fun (Position (_, i)) -> i), (fun i (Position (k, _)) -> Position (k, i))

(* Constructors

   Simple constructor functions for commonly constructed
   types, in this case an initial state for a new traversal. *)

let private createTraversal meth path =
    Traversal (
        Invariant (meth), 
        State (
            Data (path, UriTemplateData Map.empty),
            Position (Root, 0)) :: [])

(* Lenses

   Lenses into the traversal state to enable deep modifications to
   the state data structure as part of the traversal. *)

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

(* Patterns

   Active Patterns for matching and extracting data from a traversal,
   particularly indicating the current state of the traversal and whether
   the traversal is still active, has reached a potential candidate route
   solution, or (in the case of neither) whether the search has exhausted
   the search space with no valid solution.

   Additional patterns are for matching against individual nodes in the
   tree, making a parser success/failure a partial active pattern with
   suitable output data. *)

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

let private (|Parsed|_|) parser path =
    match run parser path with
    | Success (data, _, p) -> Some (data, path.Substring (int p.Index))
    | _ -> None

(* Projection

   Functions to extract potential matches from the compiled graph,
   finding a matching pipeline (perhaps) when faced with a candidate
   node, or finding the next relevant edge when determining the next
   candidate node to evaluate (remembering that edges store the parser
   which determines whether the edge can be traversed). *)

let private tryFindPipe key meth =
        Graph.findNode key
     >> function | _, Endpoints endpoints ->
                    List.tryPick (fun node ->
                        match node with
                        | Endpoint (Methods m, pipe) when List.exists ((=) meth) m -> Some pipe
                        | Endpoint (All, pipe) -> Some pipe
                        | _ -> None) endpoints
                 | _ ->
                    None

let private tryFindEdge key order =
        Graph.successors key
     >> function | Some edges ->
                    List.tryPick (fun edge ->
                        match edge with
                        | key', Edge (parser, order') when order = order' -> Some (key', Edge (parser, order))
                        | _ -> None) edges
                 | _ ->
                    None

(* State

   Functions for modifying the traversal state based on logical
   operation (capturing data and moving "down" a level within the tree,
   rejecting a match and moving to the next sibling, or abandoning a match
   and moving to the next sibling of the current parent).

   We implicitly handle data state by ensuring that captured data is stored
   within the stack rather than attempting to track it separately. *)

let private capture key data path =
    function | state :: states ->
                (state
                 |> key ^= keyLens
                 |> 0 ^= orderLens
                 |> (+) data ^%= dataLens
                 |> path ^= pathLens) :: state :: states
             | _ -> []

let private reject =
    function | state :: states -> (((+) 1) ^%= orderLens) state :: states
             | _ -> []

let private abandon =
    function | _ :: state :: states -> (((+) 1) ^%= orderLens) state :: states
             | _ -> []

(* Traversal

   The traversal of the routing graph, a depth first search with
   backtracking, performing a search which must exhaust the path and
   match the method to a pipeline to succeed, any other result being
   a matching failure.

   Additionally we record the execution to the recording state when that
   state exists (an inspector is a preceding part of the running
   pipeline). *)

let rec private traverse graph traversal =
    match traversal with
    | Candidate (meth, data, key) ->
        match tryFindPipe key meth (graph ^. graphLens) with
        | Some pipe -> completionSuccess key data pipe
        | _ -> completionFailure key graph traversal
    | Progression (path, key, order) ->
        match tryFindEdge key order (graph ^. graphLens) with
        | Some (key', Edge (parser, _)) ->
            match path with
            | Parsed parser (data, path') -> matchSuccess key' data path' graph traversal
            | _ -> matchFailure key' graph traversal
        | _ ->
            matchMiss graph traversal
    | _ ->
        Freya.init Unmatched

and private completionSuccess key data pipe =
        recordCompletionSuccess key
     *> Freya.init (Matched (data, pipe))

and private completionFailure key graph traversal =
        recordCompletionFailure key
     *> traverse graph ((abandon ^%= Traversal.StateLens) traversal)

and private matchSuccess key data path graph traversal =
        recordMatchSuccess key
     *> traverse graph ((capture key data path ^%= Traversal.StateLens) traversal)

and private matchFailure key graph traversal =
        recordMatchFailure key
     *> traverse graph ((reject ^%= Traversal.StateLens) traversal)

and private matchMiss graph traversal =
        traverse graph ((abandon ^%= Traversal.StateLens) traversal)

(* Search

   An invoked traversal of the graph, supplying the method and path
   from the Freya state to create new traversal state, and running the
   traversal. *)

let private search graph =
        createTraversal <!> (!. Request.meth) <*> (!. Request.path)
    >>= traverse graph

(* Execution

   Run a search on the routing graph. In the case of a match, write
   any captured data to the state to be interrogated later through
   the routing lenses, and return the value of executing the matched
   pipeline.

   In the case of a non-match, fall through to whatever follows the
   router instance. *)

let execute graph =
        search graph
    >>= function | Matched (data, pipe) -> (Route.data .?= data) *> pipe
                 | Unmatched -> next