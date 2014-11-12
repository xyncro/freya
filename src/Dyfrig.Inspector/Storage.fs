[<AutoOpen>]
module internal Dyfrig.Inspector.Storage

open System
open Aether
open Aether.Operators

(* Types *)

type StorageConfiguration =
    { BufferSize: int }

type StorageState =
    { Logs: StorageLogEntry seq }

and StorageLogEntry =
    { Id: Guid
      Timestamp: DateTime
      Data: Map<string, obj> }

type StorageProtocol =
    | Create of Guid
    | Update of Guid * StorageLogEntryUpdate
    | Read of Guid * AsyncReplyChannel<StorageLogEntry option>

and StorageLogEntryUpdate =
    StorageLogEntry -> StorageLogEntry

(* Lenses *)

let private logsLens =
    (fun x -> x.Logs), (fun l x -> { x with Logs = l })

let private dataLens =
    (fun x -> x.Data), (fun d x -> { x with Data = d })

let itemPLens<'a> k =
    dataLens >-?> mapPLens k <?-> boxIso<'a>

(* Constructors *)

let private storageState () =
    { Logs = Seq.empty }

let private storageLogEntry id =
    { Id = id
      Timestamp = DateTime.UtcNow
      Data = Map.empty }

(* Protocol Handlers *)

let private initialize id config =
    modL logsLens (Seq.append [ storageLogEntry id ] >> Seq.truncate config.BufferSize)

let private update id f =
    modL logsLens (Seq.map (function | l when l.Id = id -> f l | l -> l))

let private read id (chan: AsyncReplyChannel<StorageLogEntry option>) =
    getL logsLens >> (Seq.tryFind (fun l -> l.Id = id)) >> chan.Reply

(* Containers *)

let storage (config: StorageConfiguration) =
    MailboxProcessor.Start (fun mbox ->
        let rec loop (state: StorageState) =
            async {
                let! proto = mbox.Receive ()

                match proto with
                | Create id -> 
                    return! loop (initialize id config state)
                | Update (id, f) -> 
                    return! loop (update id f state)
                | Read (id, chan) ->
                    read id chan state
                    return! loop state }

        loop (storageState ()))