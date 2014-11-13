[<AutoOpen>]
module Dyfrig.Inspector.Pipeline

open System
open Dyfrig.Core
open Dyfrig.Http
open Dyfrig.Pipeline

(* Types *)

type InspectorConfiguration =
    { Path: string
      History: int
      Inspectors: Inspector list }

(* Pipeline *)

let inspector config : Pipeline =
    let displayConfig = { DisplayConfiguration.Inspectors = config.Inspectors }
    let storageConfig = { StorageConfiguration.BufferSize = config.History }

    let storage = storage (storageConfig)

    owin {
        let! path = getLM Request.path

        match path with
        | x when x = config.Path -> return! display storage displayConfig
        | _ -> return! store storage }