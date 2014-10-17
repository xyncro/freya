[<AutoOpen>]
module Dyfrig.Router.Types

open Dyfrig.Http
open Dyfrig.Pipeline

type Routes =
    Route list

and Route =
    { Method: RouteMethod
      Path: string
      Pipeline: Pipeline }

and RouteMethod =
    | Any
    | Methods of Method list 