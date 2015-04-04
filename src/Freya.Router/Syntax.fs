//----------------------------------------------------------------------------
//
// Copyright (c) 2014
//
//    Ryan Riley (@panesofglass) and Andrew Cherry (@kolektiv)
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
//----------------------------------------------------------------------------

[<AutoOpen>]
module Freya.Router.Syntax

(* Custom Operations

   Custom syntax operators used in the FreyaRouter computation
   expression. Custom syntax operators are used to register pipelines based
   on matching either the path, or the path and query with the given URI
   Template. *)

type FreyaRouterBuilder with

    (* Paths *)

    [<CustomOperation ("route", MaintainsVariableSpaceUsingBind = true)>]
    member x.Route (r, meth, template, pipeline) =
        x.Update (r, (fun x ->
            { Method = meth
              Specification = Path
              Template = template
              Pipeline = pipeline } :: x))

    [<CustomOperation ("routeWithQuery", MaintainsVariableSpaceUsingBind = true)>]
    member x.RouteWithQuery (r, meth, template, pipeline) =
        x.Update (r, (fun x ->
            { Method = meth
              Specification = PathAndQuery
              Template = template
              Pipeline = pipeline } :: x))

    (* Utility *)

    [<CustomOperation ("including", MaintainsVariableSpaceUsingBind = true)>]
    member x.Including (r, routes) =
        x.Combine (r, routes)