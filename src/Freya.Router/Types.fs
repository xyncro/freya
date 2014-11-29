[<AutoOpen>]
module Freya.Router.Types

open Freya.Pipeline
open Freya.Typed

(* Types *)

type FreyaRoute =
    { Method: FreyaRouteMethod
      Path: string
      Pipeline: FreyaPipeline }

and FreyaRouteMethod =
    | All
    | Methods of Method list

type FreyaRouteData =
    Map<string, string>
