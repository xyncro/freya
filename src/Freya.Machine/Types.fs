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
module Freya.Machine.Types

open System
open Freya.Core

(* Functions

   Common type aliases for core functions used throughout
   Machine as basic node types. *)

type FreyaMachineUnary =
    Freya<unit>

type FreyaMachineBinary =
    Freya<bool>

(* Configuration *)

type FreyaMachineConfiguration =
    { Data: Map<string, obj> }

    static member DataLens =
        (fun x -> x.Data), (fun d x -> { x with Data = d })

type FreyaMachineConfigurationMetadata =
    { Configurable: bool
      Configured: bool }

(* Graph

   Types used for defining elements of graphs, common to multiple
   graph definition types. Node and edge references are defined with
   simple string keys, relying on regular comparison. *)

type FreyaMachineRef =
    | Start
    | Finish
    | Ref of string

type FreyaMachineRefPair =
    | Pair of FreyaMachineRef * FreyaMachineRef

(* Definition

   Types representing the definition used to produce an execution
   graph, using a standard nodes and edges representation. Operations on
   the graph are represented as being able to succeed, as a mapping of
   definition graph -> definition graph, or fail as an error. *)

type FreyaMachineGraph =
    { Nodes: Map<FreyaMachineRef, FreyaMachineNode option>
      Edges: Map<FreyaMachineRefPair, FreyaMachineEdge> }

    static member NodesLens =
        (fun x -> x.Nodes), (fun n x -> { x with Nodes = n })

    static member EdgesLens =
        (fun x -> x.Edges), (fun e x -> { x with Edges = e })

and FreyaMachineNode =
    | Unary of FreyaMachineUnaryNode
    | Binary of FreyaMachineBinaryNode

and FreyaMachineUnaryNode =
    FreyaMachineConfiguration -> FreyaMachineConfigurationMetadata * FreyaMachineUnary

and FreyaMachineBinaryNode =
    FreyaMachineConfiguration -> FreyaMachineConfigurationMetadata * FreyaMachineBinary

and FreyaMachineEdge =
    | Value of bool option

type FreyaMachineGraphOperation =
    FreyaMachineGraph -> Choice<FreyaMachineGraph, string>

(* Extension

   Types supporting the extension of the basic (empty) machine graph
   through applying a set of operations to map the graph in to a new form.
   Names and dependencies are used to ensure some level of preconditional
   safety before operations from multiple extensions are applied.

   See the commentary around Dependency.fs more. *)

[<CustomEquality>]
[<CustomComparison>]
type FreyaMachineExtension =
    { Name: string
      Dependencies: Set<string>
      Operations: FreyaMachineGraphOperation list }

    static member private Comparable (x: FreyaMachineExtension) =
        x.Name.ToLowerInvariant ()

    override x.Equals y =
        equalsOn FreyaMachineExtension.Comparable x y

    override x.GetHashCode () =
        hashOn FreyaMachineExtension.Comparable x

    interface IComparable with

        member x.CompareTo y =
            compareOn FreyaMachineExtension.Comparable x y

(* Computation Expression *)

type FreyaMachine =
    FreyaMachineSpecification -> unit * FreyaMachineSpecification

and FreyaMachineSpecification =
    { Configuration: FreyaMachineConfiguration
      Extensions: Set<FreyaMachineExtension> }

    static member ConfigurationLens =
        (fun x -> x.Configuration), (fun d x -> { x with Configuration = d })

    static member ExtensionsLens =
        (fun x -> x.Extensions), (fun e x -> { x with Extensions = e })