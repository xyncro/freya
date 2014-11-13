module Dyfrig.Machine.Inspector

open Dyfrig.Inspector
open Dyfrig.Machine

let private renderMachine (data: MachineInspection) =
    sprintf "Machine execution count: %i" data.Execution.Length

let private render (data: Map<string, obj>) =
    Map.tryFind machineInspectionKey data
    |> Option.bind (function | :? MachineInspection as x -> Some x | _ -> None)
    |> Option.map renderMachine

let machineInspector : Inspector =
    { Render = render }