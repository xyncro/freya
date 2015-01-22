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
module internal Freya.Machine.Extensions.Http.Cors.Decisions

open Freya.Core
open Freya.Machine
open Freya.Machine.Extensions.Http
open Freya.Machine.Operators
open Freya.Types.Http
open Freya.Types.Http.Cors

(* Decisions *)

let private systemDecision f =
    Binary (fun config -> unconfigurable, f config)

let private corsEnabled config =
    Cors.enabled
        (tryGetConfiguration Configuration.CorsOriginsSupported config |> Option.orElse Defaults.corsOriginsSupported)

let private corsOriginAllowed config =
    Cors.originAllowed
        (getPLM Request.Headers.origin)
        (tryGetConfiguration Configuration.CorsOriginsSupported config |> Option.orElse Defaults.corsOriginsSupported)

let private corsOptions _ =
    Cors.options
        (getLM Request.meth)

let private corsPreflight _ =
    Cors.isPreflight
        (getPLM Request.Headers.accessControlRequestMethod)

(* Graph *)

let operations =
    [ Ref Decisions.CorsEnabled                        =.        systemDecision corsEnabled
      Ref Decisions.CorsOptions                        =.        systemDecision corsOptions
      Ref Decisions.CorsOriginAllowed                  =.        systemDecision corsOriginAllowed
      Ref Decisions.CorsPreflight                      =.        systemDecision corsPreflight
    
      Ref Decisions.EntityLengthValid                  >/        Ref Decisions.MethodOptions
      Ref Decisions.EntityLengthValid                  >+        Ref Decisions.CorsEnabled
      Ref Decisions.CorsEnabled                        >+        Ref Decisions.CorsOriginAllowed
      Ref Decisions.CorsEnabled                        >-        Ref Decisions.MethodOptions
      Ref Decisions.CorsOriginAllowed                  >+        Ref Decisions.CorsOptions
      Ref Decisions.CorsOriginAllowed                  >-        Ref Decisions.MethodOptions
      Ref Decisions.CorsOptions                        >+        Ref Decisions.CorsPreflight
      Ref Decisions.CorsOptions                        >-        Ref Operations.CorsActual
      Ref Decisions.CorsPreflight                      >+        Ref Operations.CorsPreflight
      Ref Decisions.CorsPreflight                      >-        Ref Operations.CorsActual ]