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
module Freya.Lenses.Http.Cors.Lenses

open Aether.Operators
open Arachne.Http.Cors
open Freya.Lenses.Http

(* Request Lenses *)

[<RequireQualifiedAccess>]
module Request =

    (* Request Header Lenses *)

    [<RequireQualifiedAccess>]
    module Headers =

        let private headerPIso key tryParse format =
            Request.headersKey key <??> (tryParse, format)

        let accessControlRequestHeaders =
            headerPIso "Access-Control-Request-Headers" AccessControlRequestHeaders.TryParse AccessControlRequestHeaders.Format

        let accessControlRequestMethod =
            headerPIso "Access-Control-Request-Method" AccessControlRequestMethod.TryParse AccessControlRequestMethod.Format

        let origin =
            headerPIso "Origin" Origin.TryParse Origin.Format

(* Response Lenses *)

[<RequireQualifiedAccess>]
module Response =

    (* Response Header Lenses *)

    [<RequireQualifiedAccess>]
    module Headers =

        let private headerPIso key tryParse format =
            Response.headersKey key <??> (tryParse, format)

        let accessControlAllowCredentials =
            headerPIso "Access-Control-Allow-Credentials" AccessControlAllowCredentials.TryParse AccessControlAllowCredentials.Format

        let accessControlAllowHeaders =
            headerPIso "Access-Control-Allow-Headers" AccessControlAllowHeaders.TryParse AccessControlAllowHeaders.Format

        let accessControlAllowMethods =
            headerPIso "Access-Control-Allow-Methods" AccessControlAllowMethods.TryParse AccessControlAllowMethods.Format

        let accessControlAllowOrigin =
            headerPIso "Access-Control-Allow-Origin" AccessControlAllowOrigin.TryParse AccessControlAllowOrigin.Format

        let accessControlExposeHeaders =
            headerPIso "Access-Control-Expose-Headers" AccessControlExposeHeaders.TryParse AccessControlExposeHeaders.Format

        let accessControlMaxAge =
            headerPIso "Access-Control-Max-Age" AccessControlMaxAge.TryParse AccessControlMaxAge.Format