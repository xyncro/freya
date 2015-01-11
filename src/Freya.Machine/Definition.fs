[<AutoOpen>]
module Freya.Machine.Definition

open Aether
open Aether.Operators

(* Types

   Types representing the definition used to produce a Machine execution
   graph, using a standard Nodes + Edges representation. Operations on
   the graph are represented as being able to succeed, as a mapping of
   DefinitionGraph -> DefinitionGraph, or fail as an error. *)

type DefinitionGraph =
    { Nodes: Map<DefinitionNodeRef, DefinitionNode option>
      Edges: Map<DefinitionEdgeRef, DefinitionEdge> }

    static member NodesLens =
        (fun x -> x.Nodes), (fun n x -> { x with Nodes = n })

    static member EdgesLens =
        (fun x -> x.Edges), (fun e x -> { x with Edges = e })

and DefinitionNode =
    | Handler of string
    | Operation of string

and DefinitionNodeRef =
    | Start
    | Finish
    | Node of string

and DefinitionEdge =
    | Value of bool option

and DefinitionEdgeRef =
    | Edge of DefinitionNodeRef * DefinitionNodeRef

type DefinitionGraphOperation =
    DefinitionGraph -> DefinitionGraphOperationResult

and DefinitionGraphOperationResult =
    | Graph of DefinitionGraph
    | Error of string

(* Constructors

   Constructors for complex types relating to definition. *)

let definitionGraph () =
    { Nodes =
        Map.ofList [ 
            Start, None
            Finish, None ]
      Edges =
        Map.ofList [
            Edge (Start, Finish), Value (None) ] }

(* Functions

   Functions for working with definition graphs within graph
   operations, not exposed externally, only asa composites within "safe"
   definition operation functions. *)

let private optionToBool =
    function | Some _ -> true
             | _ -> false

let private containsNode nodeRef =
    getPL (DefinitionGraph.NodesLens >-?> mapPLens nodeRef) >> optionToBool

let private setNode nodeRef node =
    modL DefinitionGraph.NodesLens (Map.add nodeRef (Some node))

let private unsetNode nodeRef =
    modL DefinitionGraph.NodesLens (Map.remove nodeRef)

let private containsEdge sourceRef destRef =
    getPL (DefinitionGraph.EdgesLens >-?> mapPLens (Edge (sourceRef, destRef))) >> optionToBool

let private setEdge sourceRef destRef edge =
    modL DefinitionGraph.EdgesLens (Map.add (Edge (sourceRef, destRef)) edge)

let private unsetEdge sourceRef destRef =
    modL DefinitionGraph.EdgesLens (Map.remove (Edge (sourceRef, destRef)))

(* Operations

   Operations (functions of type DefinitionGraphOperation)
   on the DefinitionGraph type, plus functions for applying operations
   sequentially (folding over operations, with correct semantics for application
   under error states). *)

let addNewDefinitionNode nodeRef node : DefinitionGraphOperation =
    function | g when not (containsNode nodeRef g) -> Graph (setNode nodeRef node g)
             | _ -> Error (sprintf "Node [%A] Exists" nodeRef)

let removeExistingDefinitionNode nodeRef : DefinitionGraphOperation =
    function | g when containsNode nodeRef g -> Graph (unsetNode nodeRef g)
             | _ -> Error (sprintf "Node [%A] Does Not Exist" nodeRef)

let addNewDefinitionEdge sourceRef destRef edge : DefinitionGraphOperation =
    function | g when not (containsEdge sourceRef destRef g) -> Graph (setEdge sourceRef destRef edge g)
             | _ -> Error (sprintf "Edge [%A -> %A] Exists" sourceRef destRef)

let removeExistingDefinitionEdge sourceRef destRef : DefinitionGraphOperation =
    function | g when containsEdge sourceRef destRef g -> Graph (unsetEdge sourceRef destRef g)
             | _ -> Error (sprintf "Edge [%A -> %A] Does Not Exist" sourceRef destRef)

let applyDefinitionOperations operations graph =
    List.fold (fun result operation ->
        match result with
        | Graph graph -> operation graph
        | Error e -> Error e) (Graph graph) operations