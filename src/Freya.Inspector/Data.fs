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

let routeInspection =
    memoM ((Option.get) <!> getPLM (Route.valuesKey "inspection"))

(* Data *)

let private recordsData =
    freya {
        let! records = listRecords

        return toJSON records }

let private recordData =
    freya {
        let! id = routeId
        let! record = getRecord id

        return Option.map toJSON record }

let private inspectionData inspectors =
    freya {
        let! id = routeId
        let! inspection = routeInspection
        let! record = getRecord id

        match Map.tryFind inspection inspectors with
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

let private root =
    sprintf "/freya/api/requests%s"

let private map =
    List.map (fun (x: FreyaInspector) -> x.Id, x) >> Map.ofList

let data config =
    freyaRouter {
        route All (root "") records
        route All (root "/:id") record
        route All (root "/:id/inspections/:inspection") (inspection (map config.Inspectors)) } |> compileFreyaRouter