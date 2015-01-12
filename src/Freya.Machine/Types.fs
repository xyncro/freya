[<AutoOpen>]
module Freya.Machine.Types

open System
open Freya.Core

(* Functions

   Common type aliases for core functions used throughout
   Machine as basic node types. *)

type MachineUnary =
    Core<unit>

type MachineBinary =
    Core<bool>

(* Configuration *)

type MachineConfiguration =
    Map<string, obj>

type MachineConfigurationMetadata =
    { Configurable: bool
      Configured: bool }

(* Graph

   Types used for defining elements of graphs, common to multiple
   graph definition types. Node and edge references are defined with
   simple string keys, relying on regular comparison. *)

type MachineNodeRef =
    | Start
    | Finish
    | Node of string

type MachineEdgeRef =
    | Edge of MachineNodeRef * MachineNodeRef

(* Definition

   Types representing the definition used to produce an execution
   graph, using a standard nodes and edges representation. Operations on
   the graph are represented as being able to succeed, as a mapping of
   definition graph -> definition graph, or fail as an error. *)

type MachineDefinitionGraph =
    { Nodes: Map<MachineNodeRef, MachineDefinitionNode option>
      Edges: Map<MachineEdgeRef, MachineDefinitionEdge> }

    static member NodesLens =
        (fun x -> x.Nodes), (fun n x -> { x with Nodes = n })

    static member EdgesLens =
        (fun x -> x.Edges), (fun e x -> { x with Edges = e })

and MachineDefinitionNode =
    | DefinitionUnary of MachineDefinitionUnary
    | DefinitionBinary of MachineDefinitionBinary

and MachineDefinitionUnary =
    MachineConfiguration -> MachineConfigurationMetadata * MachineUnary

and MachineDefinitionBinary =
    MachineConfiguration -> MachineConfigurationMetadata * MachineBinary

and MachineDefinitionEdge =
    | Value of bool option

type MachineDefinitionOperation =
    MachineDefinitionGraph -> Choice<MachineDefinitionGraph, string>

(* Extension

   Types supporting the extension of the basic (empty) machine graph
   through applying a set of operations to map the graph in to a new form.
   Names and dependencies are used to ensure some level of preconditional
   safety before operations from multiple extensions are applied.

   See the commentary around Dependency.fs more. *)

[<CustomEquality>]
[<CustomComparison>]
type MachineExtension =
    { Name: string
      Dependencies: Set<string>
      Operations: MachineDefinitionOperation list }

    static member private Comparable (x: MachineExtension) =
        x.Name.ToLowerInvariant ()

    override x.Equals y =
        equalsOn MachineExtension.Comparable x y

    override x.GetHashCode () =
        hashOn MachineExtension.Comparable x

    interface IComparable with

        member x.CompareTo y =
            compareOn MachineExtension.Comparable x y

(* Computation Expression *)

type Machine =
    MachineDefinition -> unit * MachineDefinition

and MachineDefinition =
    { Configuration: MachineConfiguration
      Extensions: Set<MachineExtension> }

    static member ConfigurationLens =
        (fun x -> x.Configuration), (fun d x -> { x with Configuration = d })

    static member ExtensionsLens =
        (fun x -> x.Extensions), (fun e x -> { x with Extensions = e })