[<AutoOpen>]
module Dyfrig.Machine.Types

open System.Globalization
open Aether
open Aether.Operators
open Dyfrig.Core
open Dyfrig.Http

(* Representations *)

type RepresentationRequest =
    { Charsets: Charset list
      Encodings: Encoding list
      MediaTypes: MediaType list
      Languages: CultureInfo list }

    static member Create charsets encodings mediaTypes languages =
        { Charsets = charsets
          Encodings = encodings
          MediaTypes = mediaTypes
          Languages = languages }

type RepresentationResponse =
    { Charset: Charset option
      Encoding: Encoding list option
      MediaType: MediaType option
      Language: CultureInfo list option
      Representation: byte [] }

    static member Default representation =
        { Charset = None
          Encoding = None
          MediaType = None
          Language = None
          Representation = representation }

(* Signatures
        
    Common monadic signatures for the building blocks of Machine
    Definitions. Represent functions that the user of Machine should implement
    when overriding the defaults. *)

type MachineAction = 
    OwinMonad<unit>

type MachineDecision = 
    OwinMonad<bool>

type MachineHandler = 
    RepresentationRequest -> OwinMonad<RepresentationResponse>

type MachineOperation =
    OwinMonad<unit>

(* Definition
        
    A Definition of a Machine, encoded as the defaults to override
    and the functions (given the previously defined Signatures) provided
    to override them. *)

type MachineDefinition =
    Map<string, MachineOverride>

and MachineOverride =
    | Action of MachineAction
    | Configuration of obj
    | Decision of MachineDecision
    | Handler of MachineHandler

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

(* Isomorphisms *)

let private boxIso<'a> : Iso<obj, 'a> =
    unbox<'a>, box

(* Lenses
        
    Partial lenses (Aether form - see https://github.com/xyncro/aether) 
    to the Machine Definition within an OWIN monad (see Dyfrig.Core),
    and to aspects of the machine definition. *)

let internal definitionPLens =
    dictPLens "dyfrig.machine.definition" <?-> boxIso<MachineDefinition>

let internal actionPLens k =
    mapPLens k <??> ((|Action|), Action)
    
let internal configurationPLens<'T> (k: string) =
    mapPLens k <??> ((|Configuration|), Configuration) <?-> boxIso<'T>
        
let internal decisionPLens k =
    mapPLens k <??> ((|Decision|), Decision)

let internal handlerPLens k =
    mapPLens k <??> ((|Handler|), Handler) 