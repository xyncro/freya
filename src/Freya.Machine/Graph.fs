[<AutoOpen>]
module internal Freya.Machine.Graph

open Aether
open Freya.Core
open Freya.Core.Operators

(* Graph

   Execution runs as a graph of nodes of specific meaning,
   Each node may (depending on type) run some kind of action and
   then provide a way of indicating which node in the graph should
   be invoked next (forming the essential characteristic of processing
   requests as a statemachine). *)

type FreyaMachineGraph =
    Map<string, FreyaMachineNode>

and FreyaMachineNode =
    | ActionNode of FreyaMachineActionNode
    | DecisionNode of FreyaMachineDecisionNode
    | HandlerNode of FreyaMachineHandlerNode
    | OperationNode of FreyaMachineOperationNode
    
and FreyaMachineActionNode =
    { Id: string
      Override: Override
      Action: FreyaMachineAction
      Next: string }

and FreyaMachineDecisionNode =
    { Id: string
      Override: Override
      Decision: FreyaMachineDecision
      True: string
      False: string }

and FreyaMachineHandlerNode =
    { Id: string
      Override: Override
      Handler: FreyaMachineHandler }

and FreyaMachineOperationNode =
    { Id: string
      Operation: FreyaMachineOperation
      Next: string }

(* Override

   Override data is used to be able to provide sensible runtime
   introspection and debugging capabilities,such as integration with future 
   Freya tracing/inspection tools. *)

and Override =
    { Allow: bool
      Overridden: bool }

(* Construction *)

// TODO: Tidy Construction

let construct (definition: FreyaMachineDefinition) nodes =
    nodes
    |> List.map (fun n ->
        match n with
        | ActionNode x ->
            x.Id,
            match x.Override.Allow, getPL (actionPLens x.Id) definition with
            | true, Some action -> 
                ActionNode { x with Action = action
                                    Override = { x.Override with Overridden = true } }
            | _ -> n
        | DecisionNode x -> 
            x.Id,
            match x.Override.Allow, getPL (decisionPLens x.Id) definition with
            | true, Some decision -> 
                DecisionNode { x with Decision = decision
                                      Override = { x.Override with Overridden = true } }
            | _ -> n
        | HandlerNode x -> 
            x.Id,
            match x.Override.Allow, getPL (handlerPLens x.Id) definition with
            | true, Some handler -> 
                HandlerNode { x with Handler = handler
                                     Override = { x.Override with Overridden = true } }
            | _ -> n
        | OperationNode x ->
            x.Id, n)
    |> Map.ofList