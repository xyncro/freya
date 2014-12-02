[<AutoOpen>]
module Freya.Inspector.Core.Extension

(* Types *)

type FreyaInspector =
    { Render: Map<string, obj> -> string option }