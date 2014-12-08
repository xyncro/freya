[<AutoOpen>]
module Freya.Router.Lenses

open Aether
open Aether.Operators
open Freya.Core

(* Keys *)

let [<Literal>] private valuesKey =
    "freya.RouterValues"

(* Lenses

   Access to values stored as part of the routing process
   are accessible via these lenses in to the OWIN state. *)

[<RequireQualifiedAccess>]
module Route =

    let values =
        dictLens<string, obj> valuesKey <--> boxIso<FreyaRouteData>

    let valuesKey key = 
        values >-?> mapPLens key