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

open Arachne.Http
open Arachne.Uri.Template

(* (Pseudo (TypeClasses

   Static inference functions to allow for type-safe overloading of arguments
   to custom syntax operations. *)

[<RequireQualifiedAccess>]
module FreyaRouteMethod =

    type Defaults =
        | Defaults

        static member inline FreyaRouteMethod (x: FreyaRouteMethod) =
            x

        static member inline FreyaRouteMethod (x: Method list) =
            Methods x

        static member inline FreyaRouteMethod (x: Method) =
            Methods [ x ]

    let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member FreyaRouteMethod: ^a -> FreyaRouteMethod) a)

    let inline set (x: 'a) =
        defaults (x, Defaults)

[<RequireQualifiedAccess>]
module UriTemplate =

    type Defaults =
        | Defaults

        static member inline UriTemplate (x: UriTemplate) =
            x

        static member inline UriTemplate (x: string) =
            UriTemplate.Parse x

    let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member UriTemplate: ^a -> UriTemplate) a)

    let inline set (x: 'a) =
        defaults (x, Defaults)

(* Custom Operations

   Custom syntax operators used in the FreyaRouter computation
   expression. Custom syntax operators are used to register pipelines based
   on matching either the path, or the path and query with the given URI
   Template. *)

type FreyaRouterBuilder with

    (* Paths *)

    [<CustomOperation ("route", MaintainsVariableSpaceUsingBind = true)>]
    member inline x.Route (r, meth, template, pipeline) =
        x.Update (r, (fun x ->
            { Method = FreyaRouteMethod.set meth
              Specification = Path
              Template = UriTemplate.set template
              Pipeline = pipeline } :: x))

    (* Utility *)

    [<CustomOperation ("including", MaintainsVariableSpaceUsingBind = true)>]
    member x.Including (r, routes) =
        x.Combine (r, routes)