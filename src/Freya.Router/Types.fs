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
module Freya.Router.Types

open Aether
open Freya.Pipeline
open Freya.Types.Http
open Freya.Types.Uri.Template

(* Routes *)

type FreyaRoute =
    { Method: FreyaRouteMethod
      Specification: FreyaRouteSpecification
      Template: UriTemplate
      Pipeline: FreyaPipeline }

    static member TemplateLens : Lens<FreyaRoute, UriTemplate> =
        (fun x -> x.Template), (fun t x -> { x with Template = t })

and FreyaRouteSpecification =
    | Path
    | PathAndQuery

and FreyaRouteMethod =
    | All
    | Methods of Method list

(* Computation Expression *)

type FreyaRouter = 
    FreyaRoute list -> unit * FreyaRoute list