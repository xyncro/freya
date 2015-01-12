[<AutoOpen>]
module Freya.Machine.Execution

(* Types

   Types defining an execution graph and supporting metadata. *)

type MachineExecutionGraph =
    { Nodes: Map<MachineNodeRef, MachineExecutionNode option> }

and MachineExecutionNode =
    | ExecutionUnary of MachineExecutionUnaryNode
    | ExecutionBinary of MachineExecutionBinaryNode

and MachineExecutionUnaryNode =
    { Unary: Unary
      Next: MachineNodeRef }

and MachineExecutionBinaryNode =
    { Binary: Binary
      True: MachineNodeRef
      False: MachineNodeRef }

(* Mapping

   Functions supporting mapping of definition graphs to execution
   graphs, a more optimised structure for runtime performance and
   simplicity of execution (requiring only lookups in a single map,
   rather than search and traversal of edges as sets in a more
   classically tractable definition graph. *)

let private findEdgeRef (g: MachineDefinitionGraph) nodeRef value =
    Map.findKey (fun (Edge (n, _)) (Value v) ->
        n = nodeRef && v = value) g.Edges

let private mapNodeUnary g nodeRef unary =
    let (Edge (_, m1)) = findEdgeRef g nodeRef None

    ExecutionUnary {
        Unary = unary
        Next = m1 }

let private mapNodeBinary g nodeRef binary =
    let (Edge (_, m1)) = findEdgeRef g nodeRef (Some true)
    let (Edge (_, m2)) = findEdgeRef g nodeRef (Some false)

    ExecutionBinary {
        Binary = binary
        True = m1
        False = m2 }

let private mapNode g nodeRef =
    function | Some (Binary x) -> Some (mapNodeBinary g nodeRef x)
             | Some (Unary x) -> Some (mapNodeUnary g nodeRef x)
             | _ -> None

let private mapPair g (nodeRef, node) =
    nodeRef, mapNode g nodeRef node

let mapDefinitionGraph (graph: MachineDefinitionGraph) =
    { MachineExecutionGraph.Nodes = 
        graph.Nodes
        |> Map.toList
        |> List.map (mapPair graph)
        |> Map.ofList }

