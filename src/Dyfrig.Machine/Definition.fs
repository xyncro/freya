namespace Dyfrig.Machine

open Aether
open Aether.Operators
open Dyfrig

type Definition =
    { Actions: Map<string, Action>
      Configuration: Map<string, obj>
      Decisions: Map<string, Decision> 
      Handlers: Map<string, Handler> }

    static member empty =
        { Actions = Map.empty
          Configuration = Map.empty
          Decisions = Map.empty
          Handlers = Map.empty }

    static member actionsPLens k =
        ((fun x -> x.Actions), (fun a x -> { x with Actions = a }))
        >-?> mapPLens k
    
    static member configPLens<'T> k =
        ((fun x -> x.Configuration), (fun c x -> { x with Configuration = c }))
        >-?> mapPLens k
        >?-> isoBoxLens<'T>
        
    static member decisionsPLens k =
        ((fun x -> x.Decisions), (fun d x -> { x with Decisions = d }))
        >-?> mapPLens k

    static member handlersPLens k =
        ((fun x -> x.Handlers), (fun h x -> { x with Handlers = h }))
        >-?> mapPLens k

and Action = 
    OwinMonad<unit>

and Decision = 
    OwinMonad<bool>

and Handler = 
    OwinMonad<byte []>
