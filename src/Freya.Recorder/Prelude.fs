[<AutoOpen>]
module internal Freya.Recorder.Prelude

open Aether

(* Isomorphisms *)

let boxIso<'a> : Iso<obj, 'a> =
    unbox<'a>, box