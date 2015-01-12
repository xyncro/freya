[<AutoOpen>]
module Freya.Machine.Execution

(* Mapping

   Functions supporting mapping of definition graphs to execution
   graphs, a more optimised structure for runtime performance and
   simplicity of execution (requiring only lookups in a single map,
   rather than search and traversal of edges as sets in a more
   classically tractable definition graph. *)

let private findEdgeRef (g: MachineDefinitionGraph) nodeRef value =
    Map.findKey (fun (Edge (n, _)) (Value v) ->
        n = nodeRef && v = value) g.Edges

let private mapUnary g c nodeRef unary =
    let configuration, unary = unary c
    let (Edge (_, m1)) = findEdgeRef g nodeRef None

    ExecutionUnary {
        Unary = unary
        Configuration = configuration
        Next = m1 }

let private mapBinary g c nodeRef binary =
    let configuration, binary = binary c
    let (Edge (_, m1)) = findEdgeRef g nodeRef (Some true)
    let (Edge (_, m2)) = findEdgeRef g nodeRef (Some false)


    ExecutionBinary {
        Binary = binary
        Configuration = configuration
        True = m1
        False = m2 }

let private mapNode g c nodeRef =
    function | Some (Unary x) -> Some (mapUnary g c nodeRef x)
             | Some (Binary x) -> Some (mapBinary g c nodeRef x)
             | _ -> None

let private mapPair g c (nodeRef, node) =
    nodeRef, mapNode g c nodeRef node

(* Creation *)

let createExecutionGraph definition =
    match createDefinitionGraph definition with
    | Choice1Of2 graph ->
        Choice1Of2 { 
            MachineExecutionGraph.Nodes = 
                graph.Nodes
                |> Map.toList
                |> List.map (mapPair graph definition.Configuration)
                |> Map.ofList }
    | Choice2Of2 e ->
        Choice2Of2 e

