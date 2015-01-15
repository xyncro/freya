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
module internal Freya.Machine.Compilation

open Freya.Core

(* Aliases

   Convenience type aliases when we have some more specific unions
   etc. in scope, in this case clashes between machine level refs
   and compilation refs, and machine level ref pairs and dependency
   pairs. *)

type Ref =
    FreyaMachineRef

type RefPair =
    FreyaMachineRefPair

(* Types

   Types defining an compilation map, and alternative formulation
   of the machine graph, optimized for runtime performance, requiring
   only map lookups. *)

type CompilationMap =
    Map<Ref, CompilationNode>

and CompilationNode =
    | Start of CompilationStartNode
    | Finish
    | Unary of CompilationUnaryNode
    | Binary of CompilationBinaryNode

and CompilationStartNode =
    { Next: Ref }

and CompilationUnaryNode =
    { Unary: FreyaMachineUnary
      Configuration: FreyaMachineConfigurationMetadata
      Next: Ref }

and CompilationBinaryNode =
    { Binary: FreyaMachineBinary
      Configuration: FreyaMachineConfigurationMetadata
      True: Ref
      False: Ref }

(* Mapping

   Functions supporting mapping of machine graphs to compilation
   maps. *)

// TODO: Consider the simplistic mapping approach, as it will throw on orphaned
// nodes, etc. Is this a good/bad thing?

let private findRefPair (graph: FreyaMachineGraph) ref value =
    printfn "%A %A" ref value

    Map.findKey (fun (RefPair.Pair (n, _)) (Value v) ->
        n = ref && v = value) graph.Edges

let private mapStart graph =
    let (RefPair.Pair (_, m1)) = findRefPair graph Ref.Start None

    Start {
        Next = m1 }

let private mapFinish _ =
    Finish

let private mapUnary graph config ref unary =
    let config, unary = unary config
    let (RefPair.Pair (_, m1)) = findRefPair graph ref None

    Unary {
        Unary = unary
        Configuration = config
        Next = m1 }

let private mapBinary graph config ref binary =
    let config, binary = binary config
    let (RefPair.Pair (_, m1)) = findRefPair graph ref (Some true)
    let (RefPair.Pair (_, m2)) = findRefPair graph ref (Some false)

    Binary {
        Binary = binary
        Configuration = config
        True = m1
        False = m2 }

let private mapNode graph config ref =
    function | FreyaMachineNode.Unary x -> mapUnary graph config ref x
             | FreyaMachineNode.Binary x -> mapBinary graph config ref x

let private mapPair graph config =
    function | Ref.Start, _ -> Ref.Start, mapStart graph
             | Ref.Finish, _ -> Ref.Finish, mapFinish graph
             | ref, Some node -> ref, mapNode graph config ref node
             | _ -> failwith "Invalid Pair"

let private mapGraph (graph: FreyaMachineGraph) configuration : CompilationMap =
    graph.Nodes
    |> Map.toList
    |> List.map (mapPair graph configuration)
    |> Map.ofList

(* Compilation

   Functions exposing compilation of machine specifications
   to compilation maps, via an intermediary stage of creating a
   machine graph from the specification. *)

let compileSpecification spec =
    match generateGraph spec with
    | Choice1Of2 graph -> Choice1Of2 (mapGraph graph spec.Configuration)
    | Choice2Of2 e -> Choice2Of2 e

