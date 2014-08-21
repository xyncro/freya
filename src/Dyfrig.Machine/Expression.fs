namespace Dyfrig.Machine

open System
open Aether
open Aether.Operators
open Dyfrig
open Dyfrig.Http

type Machine = 
    MachineDefinition -> unit * MachineDefinition

and MachineDefinition =
    { Actions: Map<string, MachineAction>
      Configuration: Map<string, obj>
      Decisions: Map<string, MachineDecision> 
      Handlers: Map<string, MachineHandler> }

    static member internal empty =
        { Actions = Map.empty
          Configuration = Map.empty
          Decisions = Map.empty
          Handlers = Map.empty }    

and MachineAction = 
    OwinMonad<unit>

and MachineDecision = 
    OwinMonad<bool>

and MachineHandler = 
    OwinMonad<byte []>

type MachineBuilder () =

    member __.Return _ : Machine =
        fun definition -> (), definition

    member __.ReturnFrom machine : Machine = 
        machine

    member __.Bind (m, k) : Machine = 
        m >> fun (result, definition) -> (k result) definition

    member x.Combine (m1, m2) : Machine = 
        x.Bind (m1, fun () -> m2)

    member internal x.Set (r, lens, value) = 
        x.Bind ((fun res -> (), setPL lens value res), fun _ -> x.ReturnFrom r)

[<AutoOpen>]
module Expression =

    let machine = MachineBuilder ()

[<AutoOpen>]
module Cache =
    
    let cache<'T> m =
        let lens =
            owinEnvPLens<'T> (string (Guid.NewGuid ()))

        owin {
            let! value = getPLM lens

            match value with
            | Some cached ->
                return cached
            | _ ->
                let! created = m
                do! setPLM lens created

                return created }

[<AutoOpen>]
module internal Lenses =

    let actionsPLens k =
        ((fun x -> x.Actions), (fun a x -> { x with Actions = a }))
        >-?> mapPLens k
    
    let configPLens<'T> k =
        ((fun x -> x.Configuration), (fun c x -> { x with Configuration = c }))
        >-?> mapPLens k
        >?-> isoLens unbox<'T> box
        
    let decisionsPLens k =
        ((fun x -> x.Decisions), (fun d x -> { x with Decisions = d }))
        >-?> mapPLens k

    let handlersPLens k =
        ((fun x -> x.Handlers), (fun h x -> { x with Handlers = h }))
        >-?> mapPLens k

    let definitionLens =
        owinEnvLens<MachineDefinition> "dyfrig.machine.definition"
