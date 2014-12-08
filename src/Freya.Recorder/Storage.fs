[<AutoOpen>]
module internal Freya.Recorder.Storage

open System
open Aether
open Aether.Operators
open Freya.Core

(* Keys *)

let [<Literal>] private requestIdKey = 
    "freya.Inspector.RequestId"

(* Types *)

type StorageProtocol =
    | Create of AsyncReplyChannel<Guid>
    | Update of Guid * (FreyaRecorderRecord -> FreyaRecorderRecord)
    | Read of Guid * AsyncReplyChannel<FreyaRecorderRecord option>
    | List of AsyncReplyChannel<FreyaRecorderRecord list>

type private StorageState =
    { Records: FreyaRecorderRecord seq }

(* Lenses *)

let internal requestIdPLens =
    environmentKeyP<Guid> requestIdKey

let private recordsLens =
    (fun x -> x.Records), (fun r x -> { x with Records = r })

let private dataLens =
    (fun x -> x.Data), (fun d x -> { x with Data = d })

let internal recordPLens<'a> k =
    dataLens >-?> mapPLens k <?-> boxIso<'a>

(* Constructors *)

let private state =
    { Records = Seq.empty }

let private entry id =
    { Id = id
      Timestamp = DateTime.UtcNow
      Data = Map.empty }

(* Protocol Handling *)

let private handle proto (state: StorageState) =
    match proto with
    | Create (chan) ->
        let id = Guid.NewGuid ()
        let state = modL recordsLens (Seq.append [ entry id ] >> Seq.truncate 10) state
        chan.Reply (id)
        state
    | Update (id, f) ->
        let state = modL recordsLens (Seq.map (function | l when l.Id = id -> f l | l -> l)) state
        state
    | Read (id, chan) ->
        let x = (getL recordsLens >> (Seq.tryFind (fun l -> l.Id = id))) state
        chan.Reply (x)
        state
    | List (chan) ->
        let x = getL recordsLens state
        chan.Reply (List.ofSeq x)
        state

(* Storage *)

let private storage () =
    MailboxProcessor.Start (fun mbox ->
        let rec loop state =
            async {
                let! proto = mbox.Receive ()
                return! loop (handle proto state) }

        loop state)

let store =
    storage ()