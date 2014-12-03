[<AutoOpen>]
module internal Freya.Recorder.Storage

open System
open Aether
open Aether.Operators
open Freya.Core
open Freya.Core.Operators
open Freya.Typed

(* Keys *)

let [<Literal>] private proxyKey = 
    "freya.InspectorProxy"

(* Types *)

type StorageProtocol =
    | Create of Guid
    | Update of Guid * FreyaRecordUpdate
    | Read of Guid * AsyncReplyChannel<FreyaRecorderRecord option>
    | List of AsyncReplyChannel<FreyaRecorderRecord seq>

and FreyaRecordUpdate =
    FreyaRecorderRecord -> FreyaRecorderRecord

type StorageProxy =
    { Update: FreyaRecordUpdate -> unit }

type private StorageState =
    { Records: FreyaRecorderRecord seq }

(* Lenses *)

let private recordsLens =
    (fun x -> x.Records), (fun r x -> { x with Records = r })

let private dataLens =
    (fun x -> x.Data), (fun d x -> { x with Data = d })

let itemPLens<'a> k =
    dataLens >-?> mapPLens k <?-> boxIso<'a>

let proxyPLens =
    dictPLens proxyKey <?-> boxIso<StorageProxy>

(* Constructors *)

let private state =
    { Records = Seq.empty }

let private entry id =
    { Id = id
      Timestamp = DateTime.UtcNow
      Data = Map.empty }

(* Handlers *)

let private create id =
    modL recordsLens (Seq.append [ entry id ] >> Seq.truncate 10)

let private update id f =
    modL recordsLens (Seq.map (function | l when l.Id = id -> f l | l -> l))

let private read id (chan: AsyncReplyChannel<FreyaRecorderRecord option>) =
    getL recordsLens >> (Seq.tryFind (fun l -> l.Id = id)) >> chan.Reply

let private list (chan: AsyncReplyChannel<FreyaRecorderRecord seq>) =
    getL recordsLens >> chan.Reply

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
                | List (chan) ->
                    list chan state
                    return! loop state }

        loop state)

let store =
    storage ()

let proxy id =
    { Update = fun update -> store.Post (Update (id, update)) }