[<AutoOpen>]
module Freya.Router.Lenses

open Aether
open Aether.Operators
open Freya.Types

(* Keys *)

let [<Literal>] private valuesKey =
    "freya.RouterValues"

(* Route Value Lenses

   Access to values stored as part of the routing process
   are accessible via these lenses in to the OWIN state. *)

[<RequireQualifiedAccess>]
module Route =

    let Values =
        dictLens<string, obj> valuesKey <--> boxIso<FreyaRouteData>

    let Value key = 
        Values >-?> mapPLens key