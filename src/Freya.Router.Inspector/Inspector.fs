module Freya.Router.Inspector

open Freya.Inspector
open Freya.Router

let private renderRouter (data: FreyaRouterRecord) =
    sprintf "Router execution count: %i" data.Execution.Length

let private render (data: Map<string, obj>) =
    Map.tryFind "freya.Machine" data
    |> Option.bind (function | :? FreyaRouterRecord as x -> Some x | _ -> None)
    |> Option.map renderRouter

let freyaRouterInspector : FreyaInspector =
    { Render = render }