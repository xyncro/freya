[<AutoOpen>]
module Freya.Inspector.Types

(* Extension *)

type FreyaInspector =
    { Render: Map<string, obj> -> string option }

(* Configuration *)

type FreyaInspectorConfiguration =
    { Path: string
      Inspectors: FreyaInspector list }