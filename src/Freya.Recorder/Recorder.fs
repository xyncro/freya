[<RequireQualifiedAccess>]
module Freya.Recorder.FreyaRecorder

open System
open Freya.Core
open Freya.Core.Operators

(* Lenses *)

let private requestIdPLens =
    Environment.optional<Guid> requestIdKey

(* Current *)

[<RequireQualifiedAccess>]
module Current =

    let initialize =
            Freya.fromAsync initialize
        =<< Freya.init () 
        >>= (.?=) requestIdPLens

    let map f =
        Option.iter (flip update f) <!> (!?.) requestIdPLens

(* History *)

[<RequireQualifiedAccess>]
module History =

    (* Inspection *)

    let list =
        Freya.fromAsync list =<< Freya.init ()

    let tryFind id =
        Freya.fromAsync read id