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
open System.Json
open Fleece
open Fleece.Operators
open Freya.Core
open Freya.Core.Operators
open Freya.Machine
open Freya.Recorder
open Freya.Router

(* Route *)

let routeId =
    memoM ((Option.get >> Guid.Parse ) <!> getPLM (Route.valuesKey "id"))

let routeKey =
    memoM ((Option.get) <!> getPLM (Route.valuesKey "key"))

(* Data *)

let private recordsData =
    freyaCore {
        let! records = listRecords

        return toJSON records }

let private recordData =
    freyaCore {
        let! id = routeId
        let! record = getRecord id

        return Option.map toJSON record }

let private inspectionData inspectors =
    freyaCore {
        let! id = routeId
        let! key = routeKey
        let! record = getRecord id

        match Map.tryFind key inspectors with
        | Some inspector -> return Option.bind inspector.Inspection.Data record
        | _ -> return None }

(* Functions *)

let private recordsGet _ =
    representJSON <!> recordsData

let private recordGet _ =
    (Option.get >> representJSON) <!> recordData

let private recordExists =
    Option.isSome <!> recordData

let private inspectionGet inspectors _=
    (Option.get >> representJSON) <!> (inspectionData inspectors)

let private inspectionExists inspectors =
    Option.isSome <!> (inspectionData inspectors)

(* Resources *)

let private records =
    freyaMachine {
        including defaults
        handleOk recordsGet } |> compileFreyaMachine

let private record =
    freyaMachine {
        including defaults
        exists recordExists
        handleOk recordGet } |> compileFreyaMachine

let private inspection inspectors =
    freyaMachine {
        including defaults
        exists (inspectionExists inspectors)
        handleOk (inspectionGet inspectors) } |> compileFreyaMachine

(* Routes *)

let private map =
    List.map (fun (x: Inspector) -> x.Key, x) >> Map.ofList

let data config =
    freyaRouter {
        route All "/freya/api/requests" records
        route All "/freya/api/requests/:id" record
        route All "/freya/api/requests/:id/:key" (inspection (map config.Inspectors)) } |> compileRouter