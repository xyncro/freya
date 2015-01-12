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

(* Types

   Types defining an compilation map, and alternative formulation
   of the machine graph, optimized for runtime performance, requiring
   only map lookups. *)

type CompilationMap =
    Map<FreyaMachineRef, CompilationNode>

and CompilationNode =
    | Unary of CompilationUnaryNode
    | Binary of CompilationBinaryNode

and CompilationUnaryNode =
    { Unary: FreyaMachineUnary
      Configuration: FreyaMachineConfigurationMetadata
      Next: FreyaMachineRef }

and CompilationBinaryNode =
    { Binary: FreyaMachineBinary
      Configuration: FreyaMachineConfigurationMetadata
      Choices: Map<bool, FreyaMachineRef> }

(* Mapping

   Functions supporting mapping of machine graphs to compilation
   maps. *)

let private findRefPair (graph: FreyaMachineGraph) ref value =
    Map.findKey (fun (FreyaMachineRefPair.Pair (n, _)) (Value v) ->
        n = ref && v = value) graph.Edges

let private mapStart graph =
    let (FreyaMachineRefPair.Pair (_, m1)) = findRefPair graph Start None

    Unary {
        Unary = Freya.init ()
        Configuration =
            { Configurable = false
              Configured = false }
        Next = m1 }

let private mapFinish _ =
    Unary {
        Unary = Freya.init ()
        Configuration =
            { Configurable = false
              Configured = false }
        Next = Finish }

let private mapUnary graph config ref unary =
    let config, unary = unary config
    let (FreyaMachineRefPair.Pair (_, m1)) = findRefPair graph ref None

    Unary {
        Unary = unary
        Configuration = config
        Next = m1 }

let private mapBinary graph config ref binary =
    let config, binary = binary config
    let (FreyaMachineRefPair.Pair (_, m1)) = findRefPair graph ref (Some true)
    let (FreyaMachineRefPair.Pair (_, m2)) = findRefPair graph ref (Some false)

    Binary {
        Binary = binary
        Configuration = config
        Choices =
            Map.ofList [
                true, m1
                false, m2 ] }

let private mapNode graph config ref =
    function | FreyaMachineNode.Unary x -> mapUnary graph config ref x
             | FreyaMachineNode.Binary x -> mapBinary graph config ref x

let private mapPair graph config =
    function | Start, _ -> Start, mapStart graph
             | Finish, _ -> Finish, mapFinish graph
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

let compile spec =
    match graph spec with
    | Choice1Of2 graph -> Choice1Of2 (mapGraph graph spec.Configuration)
    | Choice2Of2 e -> Choice2Of2 e

