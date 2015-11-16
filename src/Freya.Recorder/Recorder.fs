[<RequireQualifiedAccess>]
module Freya.Recorder.FreyaRecorder

open System
open Freya.Core
open Freya.Core.Operators

(* Current *)

[<RequireQualifiedAccess>]
module Current =

    (* Lenses *)

    let private requestId_ =
        Environment.value_<Guid> "freya.Inspector.RequestId"

    (* Functions *)

    let initialize =
            Some <!> (Freya.fromAsync Storage.initialize =<< Freya.init ())
        >>= (.=) requestId_

    let map f =
        Option.iter (flip Storage.update f) <!> (!.) requestId_

(* History *)

[<RequireQualifiedAccess>]
module History =

    (* Functions *)

    let list =
        Freya.fromAsync Storage.list =<< Freya.init ()

    let tryFind id =
        Freya.fromAsync Storage.read id