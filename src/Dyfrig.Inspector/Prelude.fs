[<AutoOpen>]
module internal Dyfrig.Inspector.Prelude

open Aether

(* Isomorphisms *)

let boxIso<'a> : Iso<obj, 'a> =
    unbox<'a>, box