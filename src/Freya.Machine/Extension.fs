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
module internal Freya.Machine.Extension

open Hekate

(* Types

   Basic type representing the results of ordering a set of Freya
   Machine Extensions. A set of extensions may not be able to be
   ordered in certain cases. *)

type FreyaMachineExtensionOrdering =
    | Ordered of FreyaMachineExtension list
    | Cyclic

(* Functions

   Functions to order a set of machine dependencies. The sort/ordering
   is essentially a variation on Kahn's algorithm, which can be more
   simply expressed here as our graph library handles to the removal
   of relevant edges as part of removing nodes.
   
   See [https://en.wikipedia.org/wiki/Topological_sorting] for details
   of topological sorting in general, and Kahn's algorithm in particular. *)

let private graph (extensions: FreyaMachineExtension list) =
    let nodes =
        extensions
        |> List.map (fun e -> e.Name, e)

    let edges =
        extensions
        |> List.map (fun e -> e.Name, Set.toList e.Dependencies)
        |> List.map (fun (n, es) -> List.map (fun e -> e, n, ()) es)
        |> List.concat

    Graph.create nodes edges

let private independent g =
    Graph.nodes g
    |> List.tryFind (fun (v, _) -> Graph.inwardDegree v g = Some 0)
    |> Option.map (fun (v, l) -> l, Graph.removeNode v g)

let rec private sort ls g =
    match independent g with
    | Some (l, g) -> sort (l :: ls) g
    | _ when Graph.isEmpty g -> Ordered ls
    | _ -> Cyclic

let private order =
    sort []

let orderExtensions =
       Set.toList 
    >> graph 
    >> order