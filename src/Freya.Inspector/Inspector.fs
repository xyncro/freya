[<AutoOpen>]
module Freya.Inspector.Inspector

open System
open Freya.Core
open Freya.Pipeline
open Freya.Typed

(* Types *)

type FreyaInspectorConfiguration =
    { Path: string
      History: int
      Inspectors: FreyaInspector list }

(* Pipeline *)

let freyaInspector config : FreyaPipeline =
    let displayConfig = { DisplayConfiguration.Inspectors = config.Inspectors }
    let storageConfig = { StorageConfiguration.BufferSize = config.History }

    let storage = storage (storageConfig)

    freya {
        let! path = getLM Request.path

        match path with
        | x when x = config.Path -> return! display storage displayConfig
        | _ -> return! store storage }
