[<AutoOpen>]
module internal Freya.Machine.Execution

(* Types

   Types defining an execution graph and supporting metadata. *)

type MachineExecutionGraph =
    { Nodes: Map<MachineNodeRef, MachineExecutionNode option> }

and MachineExecutionNode =
    | ExecutionUnary of MachineExecutionUnary
    | ExecutionBinary of MachineExecutionBinary

and MachineExecutionUnary =
    { Unary: MachineUnary
      Configuration: MachineConfigurationMetadata
      Next: MachineNodeRef }

and MachineExecutionBinary =
    { Binary: MachineBinary
      Configuration: MachineConfigurationMetadata
      True: MachineNodeRef
      False: MachineNodeRef }

(* Mapping

   Functions supporting mapping of definition graphs to execution
   graphs, a more optimised structure for runtime performance and
   simplicity of execution (requiring only lookups in a single map,
   rather than search and traversal of edges as sets in a more
   classically tractable definition graph. *)

let private findEdgeRef (graph: MachineDefinitionGraph) nodeRef value =
    Map.findKey (fun (Edge (n, _)) (Value v) ->
        n = nodeRef && v = value) graph.Edges

let private mapUnary graph config nodeRef unary =
    let config, unary = unary config
    let (Edge (_, m1)) = findEdgeRef graph nodeRef None

    ExecutionUnary {
        Unary = unary
        Configuration = config
        Next = m1 }

let private mapBinary graph config nodeRef binary =
    let config, binary = binary config
    let (Edge (_, m1)) = findEdgeRef graph nodeRef (Some true)
    let (Edge (_, m2)) = findEdgeRef graph nodeRef (Some false)


    ExecutionBinary {
        Binary = binary
        Configuration = config
        True = m1
        False = m2 }

let private mapNode graph config nodeRef =
    function | Some (DefinitionUnary x) -> Some (mapUnary graph config nodeRef x)
             | Some (DefinitionBinary x) -> Some (mapBinary graph config nodeRef x)
             | _ -> None

let private mapPair graph config (nodeRef, node) =
    nodeRef, mapNode graph config nodeRef node

let private mapGraph (graph: MachineDefinitionGraph) configuration =
     { MachineExecutionGraph.Nodes =
        graph.Nodes
        |> Map.toList
        |> List.map (mapPair graph configuration)
        |> Map.ofList }

(* Creation *)

let createExecutionGraph definition =
    match createDefinitionGraph definition with
    | Choice1Of2 graph -> Choice1Of2 (mapGraph graph definition.Configuration)
    | Choice2Of2 e -> Choice2Of2 e

