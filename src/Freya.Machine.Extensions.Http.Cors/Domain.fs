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
module internal Freya.Machine.Extensions.Http.Cors.Domain

open Arachne.Http
open Arachne.Http.Cors
open Freya.Core
open Freya.Core.Operators
open Freya.Lenses.Http.Cors

(* CORS

   Logic for negotiating decisions based on CORS (Cross-Origin Resource Sharing),
   properly supporting per-resource responses based on genuinely allowable requests,
   available methods outside of the simple set, etc. *)

[<RequireQualifiedAccess>]
module Cors =

    (* Decisions *)

    let enabled corsOriginsSupported =
        Freya.init (Option.isSome corsOriginsSupported)

    let originAllowed origin corsOriginsSupported =
            fun origin origins ->
                match origin, origins with
                | Some _, AccessControlAllowOriginRange.Any -> true
                | Some (Origin (OriginListOrNull.Origins (x :: []))),
                  Origins (OriginListOrNull.Origins xs) -> List.exists ((=) x) xs
                | _ -> false
        <!> origin   
        <*> corsOriginsSupported

    let options meth =
            (=) OPTIONS 
        <!> meth

    let isPreflight accessControlRequestMethod =
            Option.isSome
        <!> accessControlRequestMethod

    (* Operations *)

    // TODO: Reinstate this logic

    let private corsHeadersAllowed accessControlRequestHeaders corsHeadersSupported =
            fun headers supported ->
                match List.forall (fun x -> List.exists ((=) x) supported) headers with
                | true -> headers
                | _ -> []
        <!> accessControlRequestHeaders
        <*> corsHeadersSupported

    let private corsMethodsAllowed accessControlRequestMethod corsMethodsSupported =
            fun meth supported ->
                match List.exists ((=) meth) supported with
                | true -> [ meth ]
                | _ -> []
        <!> accessControlRequestMethod
        <*> corsMethodsSupported

    let private accessControlAllowMethods corsMethodsAllowed =
            (AccessControlAllowMethods >> Some) <!> corsMethodsAllowed
        >>= Freya.Lens.set Response.Headers.accessControlAllowMethods_

    let private accessControlExposeHeaders corsHeadersExposed =
            (AccessControlExposeHeaders >> Some) <!> corsHeadersExposed
        >>= Freya.Lens.set Response.Headers.accessControlExposeHeaders_

    let private accessControlAllowHeaders corsHeadersAllowed =
            (AccessControlAllowHeaders >> Some) <!> corsHeadersAllowed
        >>= Freya.Lens.set Response.Headers.accessControlAllowHeaders_

    let private accessControlAllowOrigin origin =
            (Origins >> AccessControlAllowOrigin >> Some) <!> origin
        >>= Freya.Lens.set Response.Headers.accessControlAllowOrigin_ 

    let actual =
        accessControlExposeHeaders

    let origin =
            accessControlAllowOrigin

    let preflight corsHeadersAllowed corsMethodsAllowed =
            accessControlAllowMethods corsMethodsAllowed
         *> accessControlAllowHeaders corsHeadersAllowed