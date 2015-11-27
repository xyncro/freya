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

open Arachne.Http.Cors
open Freya.Core
open Freya.Machine
open Freya.Machine.Extensions.Http

(* Type Classes

   Static inference functions to allow for type-safe overloading of arguments
   to custom syntax operations. *)

[<RequireQualifiedAccess>]
module AccessControlAllowOriginRange =

    type Defaults =
        | Defaults

        static member inline AccessControlAllowOriginRange (x: Freya<AccessControlAllowOriginRange>) =
            x

        static member inline AccessControlAllowOriginRange (x: AccessControlAllowOriginRange) =
            Freya.init x

    let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member AccessControlAllowOriginRange: ^a -> Freya<AccessControlAllowOriginRange>) a)

    let inline infer (x: 'a) =
        defaults (x, Defaults)

[<RequireQualifiedAccess>]
module Strings =

    type Defaults =
        | Defaults

        static member inline Strings (x: Freya<string list>) =
            x

        static member inline Strings (x: string list) =
            Freya.init x

        static member inline Strings (x: string) =
            Freya.init [ x ]

    let inline defaults (a: ^a, _: ^b) =
            ((^a or ^b) : (static member Strings: ^a -> Freya<string list>) a)

    let inline infer (x: 'a) =
        defaults (x, Defaults)

(* Custom Operations

   Custom syntax operators used in the FreyaMachine computation
   expression. Custom syntax operators are used heavily and are the
   configuration mechanism for configuring a machine resource. *)

type FreyaMachineBuilder with

    (* Properties *)

    [<CustomOperation (Properties.CorsHeadersExposed, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.CorsHeadersExposed (m, exposed) = 
        x.Map (m, Configuration.add Properties.CorsHeadersExposed (Strings.infer exposed))

    [<CustomOperation (Properties.CorsHeadersSupported, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.CorsHeadersSupported (m, supported) = 
        x.Map (m, Configuration.add Properties.CorsHeadersSupported (Strings.infer supported))

    [<CustomOperation (Properties.CorsMethodsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.CorsMethodsSupported (m, supported) = 
        x.Map (m, Configuration.add Properties.CorsMethodsSupported (Methods.infer supported))

    [<CustomOperation (Properties.CorsOriginsSupported, MaintainsVariableSpaceUsingBind = true)>]
    member inline x.CorsOriginsSupported (m, origins) = 
        x.Map (m, Configuration.add Properties.CorsOriginsSupported (AccessControlAllowOriginRange.infer origins))