[<AutoOpen>]
module internal Freya.Inspector.Core.Prelude

open Aether

(* Isomorphisms *)

let boxIso<'a> : Iso<obj, 'a> =
    unbox<'a>, box