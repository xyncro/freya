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

let private setConfig<'a> key a =
    Lens.map FreyaMachineSpecification.ConfigurationLens (setConfig<'a> key a)

(* Custom Operations

   Custom syntax operators used in the FreyaMachine computation
   expression. Custom syntax operators are used heavily and are the
   configuration mechanism for configuring a machine resource. *)

type FreyaMachineBuilder with

    (* Configuration *)

    [<CustomOperation (Configuration.CorsHeadersExposed, MaintainsVariableSpaceUsingBind = true)>]
    member x.CorsHeadersExposed (monad, headers: Freya<string list>) = 
        x.Map (monad, setConfig Configuration.CorsHeadersExposed headers)

    [<CustomOperation (Configuration.CorsHeadersSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.CorsHeadersSupported (monad, headers: Freya<string list>) = 
        x.Map (monad, setConfig Configuration.CorsHeadersSupported headers)

    [<CustomOperation (Configuration.CorsMethodsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.CorsMethodsSupported (monad, methods: Freya<Method list>) = 
        x.Map (monad, setConfig Configuration.CorsMethodsSupported methods)

    [<CustomOperation (Configuration.CorsOriginsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member x.CorsOriginsSupported (monad, origins: Freya<AccessControlAllowOriginRange>) = 
        x.Map (monad, setConfig Configuration.CorsOriginsSupported origins)