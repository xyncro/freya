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
module Freya.Router.Traversal

open Aether
open Aether.Operators
open Freya.Types.Uri.Template
open Hekate

(* Types

   Types used to model the state of a traversal of the compiled routing
   graph, effectively a depth first search with reliable "capturing" of
   state through traversal, with appropriate reversion on failure of
   a traversal stage.
   
   Lenses are used to more succinctly allow modification of the nested
   data structure representing a traversal. *)

type private Traversal =
    { State: TraversalState list }

    static member StateLens : Lens<Traversal, TraversalState list> =
        (fun x -> x.State), (fun s x -> { x with State = s })

and private TraversalState =
    { Position: TraversalPosition
      Data: TraversalData }

    static member PositionLens : Lens<TraversalState, TraversalPosition> =
        (fun x -> x.Position), (fun p x -> { x with Position = p })

    static member DataLens : Lens<TraversalState, TraversalData> =
        (fun x -> x.Data), (fun d x -> { x with Data = d })

and private TraversalPosition =
    { Key: string
      Order: int }

    static member KeyLens : Lens<TraversalPosition, string> =
        (fun x -> x.Key), (fun k x -> { x with Key = k })

    static member OrderLens : Lens<TraversalPosition, int> =
        (fun x -> x.Order), (fun o x -> { x with Order = o })

and private TraversalData =
    { Path: string
      Data: UriTemplateData }

    static member PathLens : Lens<TraversalData, string> =
        (fun x -> x.Path), (fun p x -> { x with Path = p })

    static member DataLens : Lens<TraversalData, UriTemplateData> =
        (fun x -> x.Data), (fun d x -> { x with Data = d })

(* Abbreviations *)

type Tr = Traversal
type TS = TraversalState
type TP = TraversalPosition
type TD = TraversalData

(* Constructors

   Constructor functions for commonly used variants of types, in this
   case a simple traversal with an initial path, and a starting point
   at the first successor of the root ("") node. *)

let private traversal path =
    { State =
        [ { Position =
              { Key = ""
                Order = 0 }
            Data =
              { Path = path
                Data = UriTemplateData Map.empty } } ] }

(* Patterns

   Active patterns to match the potential termination/continuation
   states that a Traversal may occupy.

   We assume that if we have recursed above our original starting state
   to an empty state that matching has failed.
   
   We assume that if the remaining path in the current state is empty
   that we have a candidate for a full match (though the current node
   must be checked for validity as a terminating node - i.e. it must
   have a value and thus be a registered route, rather than an intermediate
   node).

   We assume that if the traversal state list is not empty then the current
   head of the state list is the active state and extract relevant properties
   of that state. *)

let private (|Failed|_|) =
    function | { State = [] } -> Some ()
             | _ -> None

let private (|Exhausted|_|) =
    function | { State = { Data = { Path = "" } } :: _ } -> Some ()
             | _ -> None

let private (|Active|_|) =
    function | { State = { Position = { Key = key; Order = order }
                           Data = { Path = path } } :: _ } -> Some (key, order, path)
             | _ -> None

(* Position *)

let private capture key path =
    Lens.map
        (Tr.StateLens)
        (function | state :: states ->
                      (state
                       |> Lens.set (TS.PositionLens >--> TP.KeyLens) key
                       |> Lens.set (TS.PositionLens >--> TP.OrderLens) 0
                       |> Lens.set (TS.DataLens >--> TD.DataLens) state.Data.Data
                       |> Lens.set (TS.DataLens >--> TD.PathLens) path)
                       :: state
                       :: states
                  | _ -> [])

let private reject =
    Lens.map
        (Tr.StateLens)
        (function | state :: states ->
                      (state
                       |> Lens.map (TS.PositionLens >--> TP.OrderLens) ((+) 1))
                       :: states
                  | _ -> [])

let private abandon =
    Lens.map
        (Tr.StateLens)
        (function | _ :: state :: states ->
                      (state
                       |> Lens.map (TS.PositionLens >--> TP.OrderLens) ((+) 1))
                       :: states
                  | _ -> [])

(* Traversal *)

let rec private traverse graph traversal =
    match traversal with
    | Failed _ ->
        // Failed to match complete path
        false
    | Exhausted _ ->
        // Potential match of complete path, if node is a terminal
        true
    | Active (key, order, path) ->
        let edge =
            graph
            |> Graph.successors key
            |> Option.bind (
                List.tryFind (
                    function | (_, { CompilationEdge.Order = o }) when o = order -> true
                             | _ -> false))

        match edge with
        | Some (key', { Part = part }) ->
            match part.Match path with
            | _, Some path' ->
                printfn "matched %A with %s giving %s" part path path'
                traverse graph (capture key' path' traversal)
            | _ ->
                printfn "failed to match %A with %s" part path
                traverse graph (reject traversal)
        | _ ->
            printfn "edge not found"
            traverse graph (abandon traversal)
    | _ ->
        failwith ""

(* Execution *)

let executeCompilation graph path =
    traverse graph (traversal path)