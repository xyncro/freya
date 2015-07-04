[<RequireQualifiedAccess>]
module Freya.Recorder.FreyaRecorder

open System
open Freya.Core
open Freya.Core.Operators

(* Current *)

[<RequireQualifiedAccess>]
module Current =

    (* Lenses *)

    let private requestIdPLens =
        Environment.optional<Guid> "freya.Inspector.RequestId"

    (* Functions *)

    let initialize =
            Freya.fromAsync Storage.initialize
        =<< Freya.init () 
        >>= (.?=) requestIdPLens

    let map f =
        Option.iter (flip Storage.update f) <!> (!?.) requestIdPLens

(* History *)

[<RequireQualifiedAccess>]
module History =

    (* Functions *)

    let list =
        Freya.fromAsync Storage.list =<< Freya.init ()

    let tryFind id =
        Freya.fromAsync Storage.read id