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

[<RequireQualifiedAccess>]
module internal Freya.Machine.Extensions.Http.Cors.Operations

open Arachne.Http.Cors
open Freya.Core
open Freya.Core.Operators
open Freya.Lenses.Http.Cors
open Freya.Machine
open Freya.Machine.Extensions.Http

(* Operations *)

let private systemOperation f =
    Some (Compile (fun config ->
        Compiled (Unary (f config), unconfigurable)))

let private corsActual config =
    Cors.actual
        ((Configuration.get Properties.CorsHeadersExposed >> Option.orElse Defaults.corsHeadersExposed) config)

let private corsOrigin _ =
    Cors.origin
        (Option.map (fun (Origin x) -> x) >> Option.get <!> (!.) Request.Headers.origin_)

let private corsPreflight config =
    Cors.preflight
        ((Configuration.get Properties.CorsHeadersSupported >> Option.orElse Defaults.corsHeadersSupported) config)
        ((Configuration.get Properties.CorsMethodsSupported >> Option.orElse Defaults.corsMethodsSupported) config)

(* Graph *)

open Freya.Machine.Operators

let operations =
    [ Operation Operations.CorsActual                  =.        systemOperation corsActual
      Operation Operations.CorsPreflight               =.        systemOperation corsPreflight
      Operation Operations.CorsOrigin                  =.        systemOperation corsOrigin
      
      Operation Operations.CorsActual                  >.        Operation Operations.CorsOrigin
      Operation Operations.CorsPreflight               >.        Operation Operations.CorsOrigin
      Operation Operations.CorsOrigin                  >.        Operation Decisions.MethodOptions ]