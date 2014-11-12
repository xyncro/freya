[<AutoOpen>]
module Dyfrig.Inspector.Pipeline

open System
open Aether
open Aether.Operators
open Dyfrig.Core
open Dyfrig.Core.Operators
open Dyfrig.Http
open Dyfrig.Pipeline

(* Types *)

type InspectorConfiguration =
    { History: int }

type internal InspectorProtocol =
    { Create: unit -> unit
      Update: (StorageLogEntry -> StorageLogEntry) -> unit
      Read: unit -> StorageLogEntry option }

(* Lenses *)

let internal protocolPLens =
    dictPLens "dyfrig.inspector" <?-> boxIso<InspectorProtocol>

(* Constructors *)

let private protocol (logs: MailboxProcessor<StorageProtocol>) id =
    { Create = fun () -> logs.Post (Create id)
      Update = fun f -> logs.Post (Update (id, f))
      Read = fun () -> logs.PostAndReply (fun c -> Read (id, c)) }

let inspector config =
    let logs = storage ({ BufferSize = config.History })

    owin {
        let id = Guid.NewGuid ()
        let protocol = protocol logs id

        do! setPLM protocolPLens protocol
        do! modPLM protocolPLens (fun p -> p.Create (); p)

        return Next }