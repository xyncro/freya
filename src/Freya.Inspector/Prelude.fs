[<AutoOpen>]
module internal Freya.Inspector.Prelude

open Aether

(* Isomorphisms *)

let boxIso<'a> : Iso<obj, 'a> =
    unbox<'a>, box
