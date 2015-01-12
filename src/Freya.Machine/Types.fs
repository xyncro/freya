[<AutoOpen>]
module Freya.Machine.Types

open System
open Freya.Core

(* Types *)

type Unary =
    Freya<unit>

type Binary =
    Freya<bool>

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
        (fun x -> x.Nodes), (fun n x -> { x with MachineDefinitionGraph.Nodes = n })

    static member EdgesLens =
        (fun x -> x.Edges), (fun e x -> { x with MachineDefinitionGraph.Edges = e })

and MachineDefinitionNode =
    | Unary of Unary
    | Binary of Binary

and MachineDefinitionEdge =
    | Value of bool option

type MachineDefinitionGraphOperation =
    MachineDefinitionGraph -> MachineDefinitionGraphOperationResult

and MachineDefinitionGraphOperationResult =
    | Graph of MachineDefinitionGraph
    | Error of string

(* Extension *)

[<CustomEquality; CustomComparison>]
type MachineExtension =
    { Name: string
      Dependencies: Set<string>
      DefinitionGraphOperations: MachineDefinitionGraphOperation list }

    static member private Comparable (x: MachineExtension) =
        x.Name, x.Dependencies

    override x.Equals y =
        equalsOn MachineExtension.Comparable x y

    override x.GetHashCode () =
        hashOn MachineExtension.Comparable x

    interface IComparable with

        member x.CompareTo y =
            compareOn MachineExtension.Comparable x y

(* Monad *)

type Machine =
    MachineDefinition -> unit * MachineDefinition

and MachineDefinition =
    { Extensions: Set<MachineExtension>
      Data: Map<string, obj> }

    static member ExtensionsLens =
        (fun x -> x.Extensions), (fun e x -> { x with Extensions = e })

    static member DataLens =
        (fun x -> x.Data), (fun d x -> { x with Data = d })