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

open Arachne.Http
open Arachne.Http.Cors
open Freya.Core
open Freya.Core.Operators
open Freya.Lenses.Http
open Freya.Lenses.Http.Cors
open Freya.Machine
open Freya.Machine.Extensions.Http
open Freya.Machine.Operators

(* Decisions *)

let private systemDecision f =
    Some (Compile (fun config -> 
        Compiled (Binary (f config), unconfigurable)))

let private corsEnabled config =
    Cors.enabled
        (tryGetConfig Configuration.CorsOriginsSupported config)

let private corsOriginAllowed config =
    Cors.originAllowed
        (!?. Request.Headers.origin)
        (tryGetConfigOrElse Configuration.CorsOriginsSupported Defaults.corsOriginsSupported config)

let private corsOptions _ =
    Cors.options
        (!. Request.meth)

let private corsPreflight _ =
    Cors.isPreflight
        (!?. Request.Headers.accessControlRequestMethod)

(* Graph *)

let operations =
    [ Operation Decisions.CorsEnabled                  =.        systemDecision corsEnabled
      Operation Decisions.CorsOptions                  =.        systemDecision corsOptions
      Operation Decisions.CorsOriginAllowed            =.        systemDecision corsOriginAllowed
      Operation Decisions.CorsPreflight                =.        systemDecision corsPreflight
    
      Operation Decisions.EntityLengthValid            >/        Operation Decisions.MethodOptions
      Operation Decisions.EntityLengthValid            >+        Operation Decisions.CorsEnabled
      Operation Decisions.CorsEnabled                  >+        Operation Decisions.CorsOriginAllowed
      Operation Decisions.CorsEnabled                  >-        Operation Decisions.MethodOptions
      Operation Decisions.CorsOriginAllowed            >+        Operation Decisions.CorsOptions
      Operation Decisions.CorsOriginAllowed            >-        Operation Decisions.MethodOptions
      Operation Decisions.CorsOptions                  >+        Operation Decisions.CorsPreflight
      Operation Decisions.CorsOptions                  >-        Operation Operations.CorsActual
      Operation Decisions.CorsPreflight                >+        Operation Operations.CorsPreflight
      Operation Decisions.CorsPreflight                >-        Operation Operations.CorsActual ]