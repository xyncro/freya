[<RequireQualifiedAccess>]
module Freya.Recorder.FreyaRecorder

open System
open Freya.Core
open Freya.Core.Operators

(* Keys *)

let [<Literal>] private requestIdKey =
    "freya.Inspector.RequestId"

(* Lenses *)

let private requestIdPLens =
    Environment.optional<Guid> requestIdKey

(* Current *)

[<RequireQualifiedAccess>]
module Current =

    let initialize =
            Freya.fromAsync Storage.initialize
        =<< Freya.init () 
        >>= (.?=) requestIdPLens

    let map f =
        Option.iter (flip Storage.update f) <!> (!?.) requestIdPLens

(* History *)

[<RequireQualifiedAccess>]
module History =

    (* Inspection *)

    let list =
        Freya.fromAsync Storage.list =<< Freya.init ()

    let tryFind id =
        Freya.fromAsync Storage.read id