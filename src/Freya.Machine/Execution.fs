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
module internal Freya.Machine.Execution

open Freya.Core
open Freya.Core.Operators
open Hekate

(* Operations

   Functions representing the execution and recording of monadic
   Machine operations, return the result of the operation when
   applicable (as in Binary operations). *)

let private start =
    freya {
        return None }

let private finish =
    freya {
        return () }

let private unary m =
    freya {
        do! m

        return None }

let private binary m =
    freya {
        let! x = m

        return Some (Edge x) }

(* Patterns

   Active patterns to make the execution logic more readable,
   extracting relevant data from finding nodes within an ExecutionGraph *)

let private (|Start|_|) =
    function | Some (Start, _) -> Some ()
             | _ -> None

let private (|Finish|_|) =
    function | Some (Finish, _) -> Some ()
             | _ -> None

let private (|Unary|_|) =
    function | Some (v, Some (Node (Unary m, _))) -> Some (v, m)
             | _ -> None

let private (|Binary|_|) =
    function | Some (v, Some (Node (Binary m, _))) -> Some (v, m)
             | _ -> None

(* Execution

   Functions for executing against an ExecutionGraph, traversing the
   graph until either a Finish node is reached, or a node is
   unreachable, whether because the current node has no matching successors,
   or because the next node can't be found. *)

let private next v l =
       Graph.successors v
    >> Option.map (List.tryFind (fun (_, l') -> l = l'))
    >> Option.map fst

let execute (graph: ExecutionGraph) =
    let rec eval node =
        freya {
            match node with
            | Some node ->
                match Graph.tryFindNode node graph with
                | Start -> return! (flip (next Start) graph) <!> start >>= eval
                | Finish -> return! finish
                | Unary (v, m) -> return! (flip (next v) graph) <!> unary m >>= eval
                | Binary (v, m) -> return! (flip (next v) graph) <!> binary m >>= eval
                | _ -> failwith (sprintf "Next Node %A Not Found" node)
            | _ ->
                failwith (sprintf "Next Node %A Not Determined" node) }

    eval (Some Start)
