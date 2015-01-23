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

open Freya.Core
open Freya.Core.Operators
open Freya.Machine
open Freya.Machine.Extensions.Http
open Freya.Machine.Operators
open Freya.Types.Http.Cors

(* Operations *)

let private systemOperation f =
    Unary (fun c ->
        unconfigurable, f c)

let private corsActual config =
    Cors.actual
        (tryGetConfigOrElse Configuration.CorsHeadersExposed Defaults.corsHeadersExposed config)

let private corsOrigin _ =
    Cors.origin
        (Option.map (fun (Origin x) -> x) >> Option.get <!> getPLM Request.Headers.origin)

let private corsPreflight config =
    Cors.preflight
        (tryGetConfigOrElse Configuration.CorsHeadersSupported Defaults.corsHeadersSupported config)
        (tryGetConfigOrElse Configuration.CorsMethodsSupported Defaults.corsMethodsSupported config)

(* Graph *)

let operations =
    [ Ref Operations.CorsActual                  =.        systemOperation corsActual
      Ref Operations.CorsPreflight               =.        systemOperation corsPreflight
      Ref Operations.CorsOrigin                  =.        systemOperation corsOrigin
      
      Ref Operations.CorsActual                  >.        Ref Operations.CorsOrigin
      Ref Operations.CorsPreflight               >.        Ref Operations.CorsOrigin
      Ref Operations.CorsOrigin                  >.        Ref Decisions.MethodOptions ]