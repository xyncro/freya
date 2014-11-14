[<AutoOpen>]
module Freya.Router.Types

open Freya.Http
open Freya.Pipeline

(* Types *)

type FreyaRoute =
    { Method: FreyaRouteMethod
      Path: string
      Pipeline: FreyaPipeline }

and FreyaRouteMethod =
    | Any
    | Methods of Method list 
