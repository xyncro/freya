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
module internal Freya.Machine.Extensions.Http.Cors.Constants


[<RequireQualifiedAccess>]
module Configuration =

    let [<Literal>] CorsHeadersExposed = "corsHeadersExposed"
    let [<Literal>] CorsHeadersSupported = "corsHeadersSupported"
    let [<Literal>] CorsMethodsSupported = "corsMethodsSupported"
    let [<Literal>] CorsOriginsSupported = "corsOriginsSupported"


[<RequireQualifiedAccess>]
module Decisions =

    let [<Literal>] CorsEnabled = "corsEnabled"
    let [<Literal>] CorsOptions = "corsOptions"
    let [<Literal>] CorsOriginAllowed = "corsOriginAllowed"
    let [<Literal>] CorsPreflight = "corsPreflight"


[<RequireQualifiedAccess>]
module Operations =

    let [<Literal>] private prefix = "set"

    let [<Literal>] CorsPreflight = prefix + "CorsPreflight"
    let [<Literal>] CorsActual = prefix + "CorsActual"
    let [<Literal>] CorsOrigin = prefix + "CorsOrigin"