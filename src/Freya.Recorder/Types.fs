[<AutoOpen>]
module Freya.Recorder.Types

open System

(* Low Level Store *)

type FreyaRecorderRecord =
    { Id: Guid
      Timestamp: DateTime
      Data: Map<string, obj> }