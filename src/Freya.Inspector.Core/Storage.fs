[<AutoOpen>]
module internal Freya.Inspector.Core.Storage

open System
open Aether
open Aether.Operators
open Freya.Core
open Freya.Core.Operators
open Freya.Pipeline
open Freya.Typed

(* Keys *)

let [<Literal>] private proxyKey = 
    "freya.InspectorProxy"

(* Types *)

type StorageProtocol =
    | Create of Guid
    | Update of Guid * FreyaInspectorEntryUpdate
    | Read of Guid * AsyncReplyChannel<FreyaInspectorEntry option>
    | ReadAll of AsyncReplyChannel<FreyaInspectorEntry seq>

and FreyaInspectorEntryUpdate =
    FreyaInspectorEntry -> FreyaInspectorEntry

type StorageProxy =
    { Update: FreyaInspectorEntryUpdate -> unit }

type private StorageState =
    { Entries: FreyaInspectorEntry seq }

(* Lenses *)

let private entriesLens =
    (fun x -> x.Entries), (fun e x -> { x with Entries = e })

let private dataLens =
    (fun x -> x.Data), (fun d x -> { x with Data = d })

let itemPLens<'a> k =
    dataLens >-?> mapPLens k <?-> boxIso<'a>

let proxyPLens =
    dictPLens proxyKey <?-> boxIso<StorageProxy>

(* Constructors *)

let private state =
    { Entries = Seq.empty }

let private entry id =
    { Id = id
      Timestamp = DateTime.UtcNow
      Data = Map.empty }

(* Handlers *)

let private create id =
    modL entriesLens (Seq.append [ entry id ] >> Seq.truncate 10)

let private update id f =
    modL entriesLens (Seq.map (function | l when l.Id = id -> f l | l -> l))

let private read id (chan: AsyncReplyChannel<FreyaInspectorEntry option>) =
    getL entriesLens >> (Seq.tryFind (fun l -> l.Id = id)) >> chan.Reply

let private readAll (chan: AsyncReplyChannel<FreyaInspectorEntry seq>) =
    getL entriesLens >> chan.Reply

(* Storage *)

let private storage () =
    MailboxProcessor.Start (fun mbox ->
        let rec loop (state: StorageState) =
            async {
                let! proto = mbox.Receive ()

                match proto with
                | Create id -> 
                    return! loop (create id state)
                | Update (id, f) -> 
                    return! loop (update id f state)
                | Read (id, chan) ->
                    read id chan state
                    return! loop state
                | ReadAll (chan) ->
                    readAll chan state
                    return! loop state }

        loop state)

let store =
    storage ()

let proxy id =
    { Update = fun update -> store.Post (Update (id, update)) }

//(* Pipeline *)
//
//let store (storage: Storage) : FreyaPipeline =
//    let i = Guid.NewGuid ()
//    let _ = storage.Post (Create i)
//
//    setPLM proxyPLens (proxy storage i) *> next
