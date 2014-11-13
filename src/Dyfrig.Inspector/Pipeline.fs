[<AutoOpen>]
module Dyfrig.Inspector.Pipeline

open System
open Dyfrig.Core
open Dyfrig.Http
open Dyfrig.Pipeline

(* Types *)

type InspectorConfiguration =
    { History: int }

(* Pipeline *)

let inspector configuration : Pipeline =
    let storageConfiguration = 
        { BufferSize = configuration.History }

    let storage = 
        storage (storageConfiguration)

    owin {
        let! path = getLM Request.path

        match path with
        | "/inspector" -> return! display storage
        | _ -> return! store storage }