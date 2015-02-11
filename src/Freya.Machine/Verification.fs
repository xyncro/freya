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
module internal Freya.Machine.Verification

open Hekate

(* Types *)

type Verification =
    | Verified of ExecutionGraph
    | Error of string

(* Combinators *)

let private (.&) v1 v2 =
    fun g ->
        match v1 g with
        | Verified g -> v2 g
        | Error e -> Error e

let private (.*) xs f =
    fun (g: ExecutionGraph) ->
        List.fold (fun s x -> ((fun _ -> s) .& f x) g) (Verified g) (xs g)

(* Projections *)

let private unaryNodes =
        Graph.nodes
     >> List.choose (function | (v, Some (Unary _)) -> Some v
                              | _ -> None)

let private binaryNodes =
        Graph.nodes
     >> List.choose (function | (v, Some (Binary _)) -> Some v
                              | _ -> None)

(* Properties *)

let private exists v g =
    Graph.tryFindNode v g
    |> function | Some _ -> Verified g
                | _ -> Error (sprintf "Node Exists: %A - Unverified" v)

let private hasNoSuccessors v g =
    Graph.successors v g
    |> function | Some [] -> Verified g
                | _ -> Error (sprintf "Node Has No Successor: %A - Unverified" v)

let private hasUnarySuccessor v g =
    Graph.successors v g
    |> function | Some ((_, None) :: []) -> Verified g
                | _ -> Error (sprintf "Node Has Unary Successor: %A - Unverified" v)

let private hasBinarySuccessors v g =
    Graph.successors v g
    |> function | Some s when List.length s = 2 -> Verified g
                | _ -> Error (sprintf "Node Has Binary Successors: %A - Unverified" v)

(* Constraints

   Functions for static verification of the eventual graph, based on
   required properties of the eventual execution graph. *)

let private start =
        exists Start
     .& hasUnarySuccessor Start

let private finish =
        exists Finish
     .& hasNoSuccessors Finish

let private unary =
    unaryNodes .* hasUnarySuccessor

let private binary =
    binaryNodes .* hasBinarySuccessors

(* Verification *)

let private verifications =
    [ start
      finish
      unary
      binary ]

let private apply g s v =
    ((fun _ -> s) .& v) g

let verify (graph: ExecutionGraph) =
    List.fold (apply graph) (Verified graph) verifications