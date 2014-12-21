[<AutoOpen>]
module Freya.Machine.Types

open Aether
open Aether.Operators
open Freya.Core
open Freya.Core.Operators
open Freya.Types.Http
open Freya.Types.Language

(* Representations *)

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

type FreyaMachineDefinition =
    Map<string, FreyaMachineOverride>

and FreyaMachineOverride =
    | Action of FreyaMachineAction
    | Configuration of obj
    | Decision of FreyaMachineDecision
    | Handler of FreyaMachineHandler

(* Signatures

    Common monadic signatures for the building blocks of Machine
    Definitions. Represent functions that the user of Machine should implement
    when overriding the defaults. *)

and FreyaMachineAction = 
    Freya<unit>

and FreyaMachineDecision = 
    Freya<bool>

and FreyaMachineHandler = 
    FreyaMachineNegotiation -> Freya<FreyaMachineRepresentation>

and FreyaMachineOperation =
    Freya<unit>

(* Patterns

    Active patterns for discriminating between varying kinds of 
    Override within a Machine Definition. *)

let internal (|Action|) =
    function | Action x -> Some x
             | _ -> None

let internal (|Configuration|) =
    function | Configuration x -> Some x 
             | _ -> None
        
let internal (|Decision|) =
    function | Decision x -> Some x
             | _ -> None

let internal (|Handler|) =
    function | Handler x -> Some x
             | _ -> None

(* Lenses

    Partial lenses (Aether form - see https://github.com/xyncro/aether) 
    to the Machine Definition within an OWIN monad (see Freya.Core),
    and to aspects of the machine definition. *)

let internal definitionPLens =
    environmentKeyPLens "freya.MachineDefinition" <?-> boxIso<FreyaMachineDefinition>

let internal actionKeyPLens k =
    mapPLens k <??> ((|Action|), Action)
    
let internal configurationKeyPLens<'T> k =
    mapPLens k <??> ((|Configuration|), Configuration) <?-> boxIso<'T>
        
let internal decisionKeyPLens k =
    mapPLens k <??> ((|Decision|), Decision)

let internal handlerKeyPLens k =
    mapPLens k <??> ((|Handler|), Handler) 

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