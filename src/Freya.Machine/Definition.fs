[<AutoOpen>]
module Freya.Machine.Definition

open Aether
open Aether.Operators

(* Constructors

   Constructors for complex types relating to definition. *)

let machineDefinitionGraph () =
    { MachineDefinitionGraph.Nodes =
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
    getPL (MachineDefinitionGraph.NodesLens >-?> mapPLens nodeRef) >> optionToBool

let private setNode nodeRef node =
    modL MachineDefinitionGraph.NodesLens (Map.add nodeRef (Some node))

let private unsetNode nodeRef =
    modL MachineDefinitionGraph.NodesLens (Map.remove nodeRef)

let private containsEdge sourceRef destRef =
    getPL (MachineDefinitionGraph.EdgesLens >-?> mapPLens (Edge (sourceRef, destRef))) >> optionToBool

let private setEdge sourceRef destRef edge =
    modL MachineDefinitionGraph.EdgesLens (Map.add (Edge (sourceRef, destRef)) edge)

let private unsetEdge sourceRef destRef =
    modL MachineDefinitionGraph.EdgesLens (Map.remove (Edge (sourceRef, destRef)))

(* Operations

   Operations (functions of type DefinitionGraphOperation)
   on the DefinitionGraph type, plus functions for applying operations
   sequentially (folding over operations, with correct semantics for application
   under error states). *)

let addNewNode nodeRef node : MachineDefinitionOperation =
    function | g when not (containsNode nodeRef g) -> Graph (setNode nodeRef node g)
             | _ -> Error (sprintf "Node [%A] Exists" nodeRef)

let removeExistingNode nodeRef : MachineDefinitionOperation =
    function | g when containsNode nodeRef g -> Graph (unsetNode nodeRef g)
             | _ -> Error (sprintf "Node [%A] Does Not Exist" nodeRef)

let addNewEdge sourceRef destRef edge : MachineDefinitionOperation =
    function | g when not (containsEdge sourceRef destRef g) -> Graph (setEdge sourceRef destRef edge g)
             | _ -> Error (sprintf "Edge [%A -> %A] Exists" sourceRef destRef)

let removeExistingEdge sourceRef destRef : MachineDefinitionOperation =
    function | g when containsEdge sourceRef destRef g -> Graph (unsetEdge sourceRef destRef g)
             | _ -> Error (sprintf "Edge [%A -> %A] Does Not Exist" sourceRef destRef)

let applyOperations operations graph =
    List.fold (fun result operation ->
        match result with
        | Graph graph -> operation graph
        | Error e -> Error e) (Graph graph) operations