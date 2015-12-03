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
//
//----------------------------------------------------------------------------

[<AutoOpen>]
module internal Freya.Inspector.Data

open System
open Arachne.Http
open Arachne.Uri.Template
open Chiron
open Freya.Core
open Freya.Core.Operators
open Freya.Machine
open Freya.Machine.Extensions.Http
open Freya.Machine.Router
open Freya.Recorder
open Freya.Router

(* Route *)

let private id =
    Freya.Memo.wrap ((Option.get >> Guid.Parse) <!> (!.) (Route.atom_ "id"))

let private extension =
    Freya.Memo.wrap (Option.get <!> (!.) (Route.atom_ "ext"))

(* Data *)

let private recordHeaders =
        List.map (fun (record: FreyaRecorderRecord) ->
            { FreyaRecorderRecordHeader.Id = record.Id
              Timestamp = record.Timestamp }) >> Json.serialize
    <!> FreyaRecorder.History.list

let private recordDetail =
        Option.map (fun (record: FreyaRecorderRecord) ->
            { Id = record.Id
              Timestamp = record.Timestamp
              Extensions = (Map.toList >> List.map fst) record.Data } |> Json.serialize)
    <!> (FreyaRecorder.History.tryFind =<< id)

let private inspectionData inspectors =
        fun record ext ->
            Map.tryFind ext inspectors
            |> Option.bind (fun i -> Option.bind i.Inspection.Extract record)
    <!> (FreyaRecorder.History.tryFind =<< id)
    <*> extension

(* Functions *)

let private recordsGet _ =
    representJson <!> recordHeaders

let private recordGet _ =
    (Option.get >> representJson) <!> recordDetail

let private recordExists =
    Option.isSome <!> recordDetail

let private inspectionGet inspectors _=
    (Option.get >> representJson) <!> (inspectionData inspectors)

let private inspectionExists inspectors =
    Option.isSome <!> (inspectionData inspectors)

(* Resources *)

let private records =
    freyaMachine {
        including defaults
        handleOk recordsGet
        mediaTypesSupported MediaType.Json }

let private record =
    freyaMachine {
        including defaults
        exists recordExists
        handleOk recordGet
        mediaTypesSupported MediaType.Json }

let private inspection inspectors =
    freyaMachine {
        including defaults
        exists (inspectionExists inspectors)
        handleOk (inspectionGet inspectors)
        mediaTypesSupported MediaType.Json }

(* Routes

   Note: More thought should be given to a more expandable API
   path namespacing approach in the near future. *)

let private root =
    "/freya/inspector/api/records"

let data config =
    let inspectors =
        config.Inspectors
        |> List.map (fun x -> x.Key, x)
        |> Map.ofList
        |> inspection

    freyaRouter {
        resource (root) records
        resource (root + "/{id}") record
        resource (root + "/{id}/extensions/{ext}") inspectors }