[<AutoOpen>]
module Freya.Machine.Types

open Aether
open Aether.Operators
open Freya.Core
open Freya.Core.Operators
open Freya.Types.Http
open Freya.Types.Language

(* Negotiation/Representation *)

type FreyaMachineNegotiation =
    { Charsets: FreyaMachineNegotiationResult<Charset>
      Encodings: FreyaMachineNegotiationResult<ContentCoding>
      MediaTypes: FreyaMachineNegotiationResult<MediaType>
      Languages: FreyaMachineNegotiationResult<LanguageTag> }

and FreyaMachineNegotiationResult<'a> =
    | Negotiated of 'a list
    | Free

type FreyaMachineRepresentation =
    { Metadata: FreyaMachineRepresentationMetadata
      Data: byte [] }

and FreyaMachineRepresentationMetadata =
    { Charset: Charset option
      Encodings: ContentCoding list option
      MediaType: MediaType option
      Languages: LanguageTag list option }

(* Definition

   A Definition of a Machine, encoded as the defaults to override
   and the functions (given the previously defined Signatures) provided
   to override them. *)

type internal FreyaMachineDefinition =
    Map<string, FreyaMachineOverride>

and internal FreyaMachineOverride =
    | Action of FreyaMachineAction
    | Configuration of obj
    | Decision of FreyaMachineDecision
    | Handler of FreyaMachineHandler

    static member ActionPIso : PIso<FreyaMachineOverride, FreyaMachineAction> =
        (function | Action a -> Some a | _ -> None), Action

    static member ConfigurationPIso : PIso<FreyaMachineOverride, obj> =
        (function | Configuration o -> Some o | _ -> None), Configuration

    static member DecisionPIso : PIso<FreyaMachineOverride, FreyaMachineDecision> =
        (function | Decision d -> Some d | _ -> None), Decision

    static member HandlerPIso : PIso<FreyaMachineOverride, FreyaMachineHandler> =
        (function | Handler h -> Some h | _ -> None), Handler

(* Signatures

    Common monadic signatures for the building blocks of Machine
    Definitions. Represent functions that the user of Machine should implement
    when overriding the defaults. *)

and internal FreyaMachineAction = 
    Freya<unit>

and internal FreyaMachineDecision = 
    Freya<bool>

and internal FreyaMachineHandler = 
    FreyaMachineNegotiation -> Freya<FreyaMachineRepresentation>

and internal FreyaMachineOperation =
    Freya<unit>

(* Monad *)

type FreyaMachine = 
    FreyaMachineDefinition -> unit * FreyaMachineDefinition

(* Graph

   Execution runs as a graph of nodes of specific meaning,
   Each node may (depending on type) run some kind of action and
   then provide a way of indicating which node in the graph should
   be invoked next (forming the essential characteristic of processing
   requests as a statemachine). *)

type internal FreyaMachineGraph =
    Map<string, FreyaMachineNode>

and internal FreyaMachineNode =
    | ActionNode of FreyaMachineActionNode
    | DecisionNode of FreyaMachineDecisionNode
    | HandlerNode of FreyaMachineHandlerNode
    | OperationNode of FreyaMachineOperationNode
    
and internal FreyaMachineActionNode =
    { Id: string
      Override: Override
      Action: FreyaMachineAction
      Next: string }

and internal FreyaMachineDecisionNode =
    { Id: string
      Override: Override
      Decision: FreyaMachineDecision
      True: string
      False: string }

and internal FreyaMachineHandlerNode =
    { Id: string
      Override: Override
      Handler: FreyaMachineHandler }

and internal FreyaMachineOperationNode =
    { Id: string
      Operation: FreyaMachineOperation
      Next: string }

(* Override

   Override data is used to be able to provide sensible runtime
   introspection and debugging capabilities,such as integration with future 
   Freya tracing/inspection tools. *)

and internal Override =
    { Allow: bool
      Overridden: bool }

(* Lenses

   Partial lenses (Aether form - see https://github.com/xyncro/aether) 
   to the Machine Definition within an OWIN monad (see Freya.Core),
   and to aspects of the machine definition. *)

let internal definitionPLens =
    environmentKeyPLens "freya.MachineDefinition" <?-> boxIso<FreyaMachineDefinition>

let internal actionKeyPLens k =
    mapPLens k <??> FreyaMachineOverride.ActionPIso
    
let internal configurationKeyPLens<'T> k =
    mapPLens k <??> FreyaMachineOverride.ConfigurationPIso <?-> boxIso<'T>
        
let internal decisionKeyPLens k =
    mapPLens k <??> FreyaMachineOverride.DecisionPIso

let internal handlerKeyPLens k =
    mapPLens k <??> FreyaMachineOverride.HandlerPIso

(* Configuration

   Typed access to dynamic configuration values at runtime. These are
   evaluated on machine execution, and so may be varied based on the
   specific resource in question (they are a general core Freya<'T>
   expression). *)

let internal configurationKey key =
    freya {
        let! value = getPLM (definitionPLens >??> configurationKeyPLens key)

        match value with
        | Some value -> return! Some <!> value
        | _ -> return None }