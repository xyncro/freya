namespace Dyfrig.Machine

open Dyfrig
open Dyfrig.Http

[<AutoOpen>]
module internal Execution =

    let execute (graph: Graph) =
        let rec traverse from =
            owin {
                match Map.find from graph with
                | Action (action, next) ->
                    do! action
                    printfn "action: %s" from
                    return! traverse next
                | Decision (decision, choices) ->
                    let! p = decision
                    printfn "decision: %s = %b" from p
                    return! traverse ((p |> function | true -> fst | _ -> snd) choices)
                | Handler handler ->
                    printfn "handler: %s" from
                    return! handler }

        traverse Decisions.ServiceAvailable

[<AutoOpen>]
module Compilation =
    
    let compileMachine (machine: Machine) : OwinMonad<bool> =
        let definition = machine MachineDefinition.empty |> snd
        let graph = construct definition

        owin {
            do! setLM definitionLens definition

            let! body = execute graph

            do! setPLM (Response.Header "Content-Length") [ string body.Length ]
            do! modLM Response.Body (fun x -> x.Write (body, 0, body.Length); x)
        
            return true }
