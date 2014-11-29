[<AutoOpen>]
module Freya.Router.Syntax

(* Custom Operations

   Custom syntax operators used in the FreyaRouter computation
   expression. Custom syntax operators are used to register routes. *)

type FreyaRouterBuilder with

    (* Routes *)

    [<CustomOperation ("route", MaintainsVariableSpaceUsingBind = true)>]
    member x.Route (r, meth, path, pipeline) =
        x.Update (r, (fun x -> { Method = meth; Path = path; Pipeline = pipeline } :: x))

    (* Utility *)

    [<CustomOperation ("including", MaintainsVariableSpaceUsingBind = true)>]
    member x.Including (r, routes) =
        x.Combine (r, routes)