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

[<RequireQualifiedAccess>]
module internal Freya.Router.Execution

open Aether
open Aether.Operators
open Arachne.Http
open Arachne.Uri.Template
open FParsec
open Freya.Core
open Freya.Core.Operators
open Freya.Lenses.Http
open Hekate

(* Types *)

type private SearchResult =
    | Matched of UriTemplateData * FreyaPipeline
    | Unmatched

type private Traversal =
    | Traversal of TraversalInvariant * TraversalState

    static member Invariant_ =
        (fun (Traversal (i, _)) -> i), (fun i (Traversal (_, s)) -> Traversal (i, s))

    static member State_ =
        (fun (Traversal (_, s)) -> s), (fun s (Traversal (i, _)) -> Traversal (i, s))

and private TraversalInvariant =
    | Invariant of Method

    static member Method_ =
        (fun (Invariant m) -> m), (fun m (Invariant (_)) -> Invariant (m))

and private TraversalState =
    | State of TraversalPosition * TraversalData

    static member Position_ =
        (fun (State (p, _)) -> p), (fun p (State (_, d)) -> State (p, d))

    static member Data_ =
        (fun (State (_, d)) -> d), (fun d (State (p, _)) -> State (p, d))

and private TraversalPosition =
    | Position of string * Compilation.CompilationKey

    static member Path_ =
        (fun (Position (p, _)) -> p), (fun p (Position (_, k)) -> Position (p, k))

    static member Key_ =
        (fun (Position (_, k)) -> k), (fun k (Position (p, _)) -> Position (p, k))

and private TraversalData =
    | Data of UriTemplateData

    static member Data_ =
        (fun (Data d) -> d), (fun d (Data (_)) -> Data (d))

(* Constructors *)

let private traversal meth path =
    Traversal (
        Invariant (meth),
        State (
            Position (path, Compilation.Root),
            Data (UriTemplateData (Map.empty))))

(* Lenses *)

let private data_ =
        Traversal.State_
   >--> TraversalState.Data_
   >--> TraversalData.Data_

let private key_ =
        Traversal.State_
   >--> TraversalState.Position_
   >--> TraversalPosition.Key_

let private path_ =
        Traversal.State_
   >--> TraversalState.Position_
   >--> TraversalPosition.Path_

(* Functions *)

let private tryMatch parser path =
    match run parser path with
    | Success (data, _, p) -> Some (data, path.Substring (int p.Index))
    | _ -> None

(* Patterns *)

let private (|Candidate|_|) =
    function | Traversal (Invariant meth,
                          State (
                              Position ("", key),
                              Data data)) -> Some (key, meth, data)
             | _ -> None

let private (|Progression|_|) =
    function | Traversal (Invariant _,
                          State (
                              Position (path, key),
                              Data _)) -> Some (key, path)

let private (|Endpoints|_|) key meth (Compilation.Graph graph) =
    match Graph.tryFindNode key graph with
    | Some (_, Compilation.Endpoints endpoints) ->
        endpoints
        |> List.filter (
           function | Compilation.Endpoint (_, All, _) -> true
                    | Compilation.Endpoint (_, Methods methods, _) when List.exists ((=) meth) methods -> true
                    | _ -> false)
        |> function | [] -> None
                    | endpoints -> Some endpoints
    | _ -> None

let private (|Successors|_|) key (Compilation.Graph graph) =
    match Graph.successors key graph with
    | Some x -> Some x
    | _ -> None

(* Traversal *)

let rec private traverse graph traversal =
    match traversal with
    | Candidate (key, meth, data) ->
        match graph with
        | Endpoints key meth endpoints ->
            endpoints
            |> List.map (fun endpoint -> endpoint, data)
        | _ -> []
    | Progression (key, path) ->
        match graph with
        | Successors (key) successors ->
            successors
            |> List.map (fun (key', Compilation.Edge parser) ->
                match tryMatch parser path with
                | Some (data', path') ->
                    let traversal' =
                        traversal
                        |> Lens.map data_ ((+) data')
                        |> Lens.set key_ key'
                        |> Lens.set path_ path'

                    traverse graph traversal'
                | _ ->
                    [])
            |> List.concat
        | _ -> []
    | _ -> []

(* Search *)

let private search graph =
        traverse graph <!> (traversal <!> (!. Request.Method_) <*> (!. Request.Path_))
    //>>= traverse graph

(* Execution

   Run a search on the routing graph. In the case of a match, write
   any captured data to the state to be interrogated later through
   the routing lenses, and return the value of executing the matched
   pipeline.

   In the case of a non-match, fall through to whatever follows the
   router instance. *)

let run x =
    printfn "%A" x
    Freya.next

let execute graph =
        search graph
    >>= function | x -> run x

//    >>= function | Matched (data, pipe) -> (Route.Data_ .?= data) *> pipe
//                 | Unmatched -> Freya.next