//----------------------------------------------------------------------------
//
// Copyright (c) 2014
//
//    Ryan Riley (@panesofglass) and Andrew Cherry (@kolektiv)
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module internal Freya.Recorder.Storage

open System
open Aether
open Aether.Operators
open Freya.Core

(* Types *)

type private StorageProtocol =
    | Create of AsyncReplyChannel<Guid>
    | Update of Guid * (FreyaRecorderRecord -> FreyaRecorderRecord)
    | Read of Guid * AsyncReplyChannel<FreyaRecorderRecord option>
    | List of AsyncReplyChannel<FreyaRecorderRecord list>

type private StorageState =
    { Records: FreyaRecorderRecord seq }

    static member records_ =
        (fun x -> x.Records), (fun r x -> { x with Records = r })

(* Constructors *)

let private state =
    { Records = Seq.empty }

let private record id =
    { Id = id
      Timestamp = DateTime.UtcNow
      Data = Map.empty }

(* Protocol Handling *)

let private handle proto (state: StorageState) =
    match proto with
    | Create (chan) ->
        let id = Guid.NewGuid ()
        let state = ((Seq.append [ record id ] >> Seq.truncate 10) ^% StorageState.records_) state
        chan.Reply (id)
        state
    | Update (id, f) ->
        let state = ((Seq.map (function | l when l.Id = id -> f l | l -> l)) ^% StorageState.records_) state
        state
    | Read (id, chan) ->
        let x = (flip (^.) StorageState.records_ >> (Seq.tryFind (fun l -> l.Id = id))) state
        chan.Reply (x)
        state
    | List (chan) ->
        let x = state ^. StorageState.records_
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

let private store =
    storage ()

(* Functions *)

let initialize () =
    store.PostAndAsyncReply (fun c -> Create (c))

let read id =
    store.PostAndAsyncReply (fun c -> Read (id, c))

let list () =
    store.PostAndAsyncReply (fun c -> List (c))
    
let update id f =
    store.Post (Update (id, f))