module Freya.Machine.Inspector

open Freya.Inspector
open Freya.Machine

let private renderMachine (data: FreyaMachineRecord) =
    sprintf "Machine execution count: %i" data.Execution.Length

let private render (data: Map<string, obj>) =
    Map.tryFind "freya.Machine" data
    |> Option.bind (function | :? FreyaMachineRecord as x -> Some x | _ -> None)
    |> Option.map renderMachine

let freyaMachineInspector : FreyaInspector =
    { Render = render }
