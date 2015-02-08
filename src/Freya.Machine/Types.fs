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
open Hekate

(* Configuration *)

type FreyaMachineConfiguration =
    { Data: Map<string, obj> }

    static member DataLens =
        (fun x -> x.Data), (fun d x -> { x with Data = d })

(* Node *)

type FreyaMachineNode =
    | Start
    | Operation of string
    | Finish

(* Node Label *)

type FreyaMachineNodeLabel =
    | Node of FreyaMachineOperation * FreyaMachineOperationMetadata

and FreyaMachineOperation =
    | Unary of Freya<unit>
    | Binary of Freya<bool>

and FreyaMachineOperationMetadata =
    { Configurable: bool
      Configured: bool }

(* Edge Label *)

type FreyaMachineEdgeLabel =
    | Edge of bool

(* Builders *)

type FreyaMachineNodeCompiler =
    | NodeCompiler of (FreyaMachineConfiguration -> (FreyaMachineOperation * FreyaMachineOperationMetadata))

(* Extension *)

[<CustomEquality>]
[<CustomComparison>]
type FreyaMachineExtension =
    { Name: string
      Dependencies: Set<string>
      Operations: FreyaMachineExtensionOperation list }

    static member private Comparable (x: FreyaMachineExtension) =
        x.Name.ToLowerInvariant ()

    override x.Equals y =
        equalsOn FreyaMachineExtension.Comparable x y

    override x.GetHashCode () =
        hashOn FreyaMachineExtension.Comparable x

    interface IComparable with

        member x.CompareTo y =
            compareOn FreyaMachineExtension.Comparable x y

and FreyaMachineExtensionOperation =
    | AddNode of FreyaMachineNode * FreyaMachineNodeCompiler option
    | RemoveNode of FreyaMachineNode
    | AddEdge of FreyaMachineNode * FreyaMachineNode * FreyaMachineEdgeLabel option
    | RemoveEdge of FreyaMachineNode * FreyaMachineNode

(* Computation Expression *)

type FreyaMachine =
    FreyaMachineSpecification -> unit * FreyaMachineSpecification

and FreyaMachineSpecification =
    { Configuration: FreyaMachineConfiguration
      Extensions: Set<FreyaMachineExtension> }

    static member ConfigurationLens =
        (fun x -> x.Configuration), (fun c x -> { x with Configuration = c })

    static member ExtensionsLens =
        (fun x -> x.Extensions), (fun e x -> { x with Extensions = e })