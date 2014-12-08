[<AutoOpen>]
module internal Freya.Inspector.Data

open System
open Aether
open Freya.Core
open Freya.Core.Operators
open Freya.Machine
open Freya.Pipeline
open Freya.Recorder
open Freya.Router

(* Mappings *)

let private mapRecord x =
    { Id = x.Id
      Timestamp = x.Timestamp
      Inspections = x.Data |> Map.toList |> List.map fst }

let private mapRequest x =
    getPL freyaRequestRecordPLens x

(* Data *)

let private id =
    (Option.get >> Guid.Parse ) <!> getPLM (Route.valuesKey "id")

let private recordsData =
    freya {
        let! records = listR ()

        return List.map mapRecord records }

let private recordData =
    freya {
        let! id = id
        let! record = getR id

        return Option.map mapRecord record }

(* Functions *)

let inline private getData data _ =
    representJSON <!> data

let private recordsGet =
    getData recordsData

let private recordGet =
    getData recordData

let private recordExists =
    Option.isSome <!> recordData

(* Resources *)

let private records =
    freyaMachine {
        including defaults
        mediaTypesSupported json
        handleOk recordsGet } |> compileFreyaMachine

let private record =
    freyaMachine {
        including defaults
        exists recordExists
        mediaTypesSupported json
        handleOk recordGet } |> compileFreyaMachine

(* Routes *)

let data (config: FreyaInspectorConfiguration) =
    freyaRouter {
        route All "/freya/inspect/api/requests" records
        route All "/freya/inspect/api/requests/:id/" record } |> compileFreyaRouter