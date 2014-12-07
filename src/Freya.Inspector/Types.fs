[<AutoOpen>]
module Freya.Inspector.Types

(* Extension *)

type FreyaInspector =
    { Render: Map<string, obj> -> string option }

(* Configuration *)

type FreyaInspectorConfiguration =
    { Inspectors: FreyaInspector list }