[<AutoOpen>]
module internal Freya.Inspector.Storage

open System
open Aether
open Aether.Operators
open Freya.Core
open Freya.Core.Operators
open Freya.Pipeline
open Freya.Typed

(* Types *)

type StorageConfiguration =
    { BufferSize: int }

type StorageState =
    { Logs: StorageLogEntry seq }

and StorageLogEntry =
    { Id: Guid
      Timestamp: DateTime
      Data: Map<string, obj> }

type Storage =
    MailboxProcessor<StorageProtocol>

and StorageProtocol =
    | Create of Guid
    | Update of Guid * StorageLogEntryUpdate
    | Read of Guid * AsyncReplyChannel<StorageLogEntry option>
    | ReadAll of AsyncReplyChannel<StorageLogEntry seq>

and StorageLogEntryUpdate =
    StorageLogEntry -> StorageLogEntry

type StorageProxy =
    { Update: StorageLogEntryUpdate -> unit }

(* Lenses *)

let private logsLens =
    (fun x -> x.Logs), (fun l x -> { x with Logs = l })

let private dataLens =
    (fun x -> x.Data), (fun d x -> { x with Data = d })

let itemPLens<'a> k =
    dataLens >-?> mapPLens k <?-> boxIso<'a>

let proxyPLens =
    dictPLens "freya.InspectorProxy" <?-> boxIso<StorageProxy>

(* Constructors *)

let private state =
    { Logs = Seq.empty }

let private entry id =
    { Id = id
      Timestamp = DateTime.UtcNow
      Data = Map.empty }

let private proxy (storage: Storage) id =
    { Update = fun update -> storage.Post (Update (id, update)) }

(* Handlers *)

let private create id configuration =
    modL logsLens (Seq.append [ entry id ] >> Seq.truncate configuration.BufferSize)

let private update id f =
    modL logsLens (Seq.map (function | l when l.Id = id -> f l | l -> l))

let private read id (chan: AsyncReplyChannel<StorageLogEntry option>) =
    getL logsLens >> (Seq.tryFind (fun l -> l.Id = id)) >> chan.Reply

let private readAll (chan: AsyncReplyChannel<StorageLogEntry seq>) =
    getL logsLens >> chan.Reply

(* Storage *)

let storage (configuration: StorageConfiguration) =
    MailboxProcessor.Start (fun mbox ->
        let rec loop (state: StorageState) =
            async {
                let! proto = mbox.Receive ()

                match proto with
                | Create id -> 
                    return! loop (create id configuration state)
                | Update (id, f) -> 
                    return! loop (update id f state)
                | Read (id, chan) ->
                    read id chan state
                    return! loop state
                | ReadAll (chan) ->
                    readAll chan state
                    return! loop state }

        loop state)

(* Pipeline *)

let store (storage: Storage) : FreyaPipeline =
    let i = Guid.NewGuid ()
    let _ = storage.Post (Create i)

    setPLM proxyPLens (proxy storage i) *> next
