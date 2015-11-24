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
module Freya.Machine.Extensions.Http.Cors.Syntax

open Aether
open Arachne.Http
open Arachne.Http.Cors
open Freya.Core
open Freya.Machine

(* Helper Functions *)

let private set<'a> key a =
    Lens.map FreyaMachineSpecification.Configuration_ (Configuration.set<'a> key (Some a))

(* Custom Operations

   Custom syntax operators used in the FreyaMachine computation
   expression. Custom syntax operators are used heavily and are the
   configuration mechanism for configuring a machine resource. *)

type FreyaMachineBuilder with

    (* Properties *)

    [<CustomOperation (Properties.CorsHeadersExposed, MaintainsVariableSpaceUsingBind = true)>]
    member x.CorsHeadersExposed (m, headers: Freya<string list>) = 
        x.Map (m, set Properties.CorsHeadersExposed headers)

    [<CustomOperation (Properties.CorsHeadersSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.CorsHeadersSupported (m, headers: Freya<string list>) = 
        x.Map (m, set Properties.CorsHeadersSupported headers)

    [<CustomOperation (Properties.CorsMethodsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.CorsMethodsSupported (m, methods: Freya<Method list>) = 
        x.Map (m, set Properties.CorsMethodsSupported methods)

    [<CustomOperation (Properties.CorsOriginsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.CorsOriginsSupported (m, origins: Freya<AccessControlAllowOriginRange>) = 
        x.Map (m, set Properties.CorsOriginsSupported origins)