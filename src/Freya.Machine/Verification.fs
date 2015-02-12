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

(* NOTE: This verification system is rather basic at the moment
   and everything down to the stylistic choice of design (perhaps
   *especially* the stylistic choice of design!) is open to
   reconsideration and suggestion.

   Ideally a fairly declarative system of constraints might be
   designed, which would possibly have relevance to being ported
   back in to the core Hekate library. *)

open Hekate

(* Types

   Type supporting the verification of execution graphs, by a set of
   graph postconditions, ensuring some level of runtime safety when
   executing a verified graph. *)

type Verification =
    | Verified of ExecutionGraph
    | Error of string

(* Operators

   Simple operator based functions for expressing the combinations and
   multiple applications of property constraints to projections of an
   execution graph. *)

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

(* Properties

   Property based verifications of singlular "facts" which should be true
   about an entity, in this case properties of nodes, though this may well be
   extended. *)

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

   Constraints defined as composites of property constraints, based
   on individual node types within execution graphs. This set of
   constraints should potentially be extended, especially to check
   for aberrant cases of the graph overall (cyclic paths, etc.) when
   Hekate supports more general algorithms over graphs. *)

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

(* Verification

   Verification of execution graphs, given some reasonable set of
   constraints. Implicitly this forms a postcondition for graph construction.
   It would be nice to find ways of making safety more available at
   compile time, but given the limitations of the type system at
   the moment, this is a reasonable compromise. *)

let private verifications =
    [ start
      finish
      unary
      binary ]

let private apply g s v =
    ((fun _ -> s) .& v) g

let verify exec =
    List.fold (apply exec) (Verified exec) verifications