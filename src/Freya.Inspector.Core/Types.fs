[<AutoOpen>]
module Freya.Inspector.Core.Types

open System

(* Low Level Store *)

type FreyaInspectorEntry =
    { Id: Guid
      Timestamp: DateTime
      Data: Map<string, obj> }