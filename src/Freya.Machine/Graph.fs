[<AutoOpen>]
module internal Freya.Machine.Graph

open Aether

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

let private actionNode x action =
    ActionNode { x with Action = action
                        Override = { x.Override with Overridden = true } }

let private action (x: FreyaMachineActionNode) def =
    match x.Override.Allow, getPL (actionKeyPLens x.Id) def with
    | true, Some action -> x.Id, actionNode x action
    | _ -> x.Id, ActionNode x

let private decisionNode x decision =
    DecisionNode { x with Decision = decision
                          Override = { x.Override with Overridden = true } }

let private decision (x: FreyaMachineDecisionNode) def =
    match x.Override.Allow, getPL (decisionKeyPLens x.Id) def with
    | true, Some decision -> x.Id, decisionNode x decision
    | _ -> x.Id, DecisionNode x

let private handlerNode x handler =
    HandlerNode { x with Handler = handler
                         Override = { x.Override with Overridden = true } }

let private handler (x: FreyaMachineHandlerNode) def =
    match x.Override.Allow, getPL (handlerKeyPLens x.Id) def with
    | true, Some handler -> x.Id, handlerNode x handler
    | _ -> x.Id, HandlerNode x

let private operation (x: FreyaMachineOperationNode) =
    x.Id, OperationNode x

let private node def =
    function | ActionNode x -> action x def
             | DecisionNode x -> decision x def
             | HandlerNode x -> handler x def
             | OperationNode x -> operation x


let construct (def: FreyaMachineDefinition) =
    List.map (node def) >> Map.ofList