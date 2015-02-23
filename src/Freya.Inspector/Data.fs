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

[<AutoOpen>]
module internal Freya.Inspector.Data

open System
open Chiron
open Freya.Core
open Freya.Core.Operators
open Freya.Machine
open Freya.Machine.Extensions.Http
open Freya.Machine.Router
open Freya.Recorder
open Freya.Router

(* Route *)

let routeId =
    Freya.memo ((Option.get >> Guid.Parse ) <!> Freya.getLensPartial (Route.valuesKey "id"))

let routeExtension =
    Freya.memo ((Option.get) <!> Freya.getLensPartial (Route.valuesKey "ext"))

(* Data *)

let private recordHeaders =
    freya {
        let! records = listRecords

        let recordHeaders =
            records
            |> List.map (fun r ->
                { FreyaRecorderRecordHeader.Id = r.Id
                  Timestamp = r.Timestamp })

        return Json.serialize recordHeaders }

let private recordDetail =
    freya {
        let! id = routeId
        let! record = getRecord id

        let recordDetail =
            record
            |> Option.map (fun r ->
                { FreyaRecorderRecordDetail.Id = r.Id
                  Timestamp = r.Timestamp
                  Extensions = r.Data |> Map.toList |> List.map fst })

        return Option.map Json.serialize recordDetail }

let private inspectionData inspectors =
    freya {
        let! id = routeId
        let! extension = routeExtension
        let! record = getRecord id

        match Map.tryFind extension inspectors with
        | Some inspector -> return Option.bind inspector.Inspection.Data record
        | _ -> return None }

(* Functions *)

let private recordsGet _ =
    representJSON <!> recordHeaders

let private recordGet _ =
    (Option.get >> representJSON) <!> recordDetail

let private recordExists =
    Option.isSome <!> recordDetail

let private inspectionGet inspectors _=
    (Option.get >> representJSON) <!> (inspectionData inspectors)

let private inspectionExists inspectors =
    Option.isSome <!> (inspectionData inspectors)

(* Resources *)

let private records =
    freyaMachine {
        including defaults
        handleOk recordsGet
        mediaTypesSupported Prelude.json } |> Machine.toPipeline

let private record =
    freyaMachine {
        including defaults
        exists recordExists
        handleOk recordGet
        mediaTypesSupported Prelude.json } |> Machine.toPipeline

let private inspection inspectors =
    freyaMachine {
        including defaults
        exists (inspectionExists inspectors)
        handleOk (inspectionGet inspectors)
        mediaTypesSupported Prelude.json } |> Machine.toPipeline

(* Routes

   Note: More thought should be given to a more expandable API
   path namespacing approach in the near future. *)

let private map =
    List.map (fun (x: FreyaInspector) -> x.Key, x) >> Map.ofList

let data config =
    let inspectors =
        inspection (map config.Inspectors)

    freyaRouter {
        resource "/freya/inspector/api/records" records
        resource "/freya/inspector/api/records/:id" record
        resource "/freya/inspector/api/records/:id/extensions/:ext" inspectors } |> Router.toPipeline