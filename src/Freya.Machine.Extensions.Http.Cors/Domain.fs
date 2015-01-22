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

open Freya.Core
open Freya.Core.Operators
open Freya.Types.Http
open Freya.Types.Http.Cors

(* CORS

   Logic for negotiating decisions based on CORS (Cross-Origin Resource Sharing),
   properly supporting per-resource responses based on genuinely allowable requests,
   available methods outside of the simple set, etc. *)

[<RequireQualifiedAccess>]
module Cors =

    (* Configuration *)

//    let private corsHeadersExposed =
//        configurationKey Configuration.CorsHeadersExposed
//
//    let private corsHeadersSupported =
//        configurationKey Configuration.CorsHeadersSupported
//
//    let private corsMethodsSupported =
//        configurationKey Configuration.CorsMethodsSupported
//
//    let private corsOriginsSupported =
//        configurationKey Configuration.CorsOriginsSupported

    (* Request *)

//    let private accessControlRequestHeaders =
//        getPLM Request.Headers.accessControlRequestHeaders
//
//    let private accessControlRequestMethod =
//        getPLM Request.Headers.accessControlRequestMethod
//
//    let private meth =
//        getLM Request.meth
//
//    let private origin =
//        getPLM Request.Headers.origin

    (* Derived *)

//    let private accessControlRequestHeaders' =
//            (function | Some (AccessControlRequestHeaders x) -> x
//                      | _ -> [])
//        <!> accessControlRequestHeaders
//
//    let private accessControlRequestMethod' =
//            (Option.map (fun (AccessControlRequestMethod x) -> x) >> Option.get)
//        <!> accessControlRequestMethod
//
//    let private corsHeadersExposed' =
//            (function | Some x -> x
//                      | _ -> [])
//        <!> corsHeadersExposed
//
//    let private corsHeadersSupported' =
//            (function | Some x -> x
//                      | _ -> [])
//        <!> corsHeadersSupported
//
//    let private corsMethodsSupported' =
//            (function | Some x -> x
//                      | _ -> [])
//        <!> corsMethodsSupported
//
//    let private origin' =
//            (Option.map (fun (Origin x) -> x) >> Option.get) 
//        <!> origin

    (* Decisions *)

    let enabled corsOriginsSupported =
            Option.isSome
        <!> corsOriginsSupported

    let originAllowed origin corsOriginsSupported =
            fun origin origins ->
                match origin, origins with
                | Some _, Some (AccessControlAllowOriginRange.Any) -> true
                | Some (Origin (OriginListOrNull.Origins (x :: []))),
                  Some (Origins (OriginListOrNull.Origins xs)) -> List.exists ((=) x) xs
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
            setPLM Response.Headers.accessControlAllowMethods
        =<< (AccessControlAllowMethods <!> corsMethodsAllowed)

    let private accessControlExposeHeaders corsHeadersExposed =
            setPLM Response.Headers.accessControlExposeHeaders
        =<< (AccessControlExposeHeaders <!> corsHeadersExposed)

    let private accessControlAllowHeaders corsHeadersAllowed =
            setPLM Response.Headers.accessControlAllowHeaders
        =<< (AccessControlAllowHeaders <!> corsHeadersAllowed)

    let private accessControlAllowOrigin origin =
            setPLM Response.Headers.accessControlAllowOrigin 
        =<< ((Origins >> AccessControlAllowOrigin) <!> origin)

    let actual =
        accessControlExposeHeaders

    let origin =
        accessControlAllowOrigin

    let preflight corsHeadersAllowed corsMethodsAllowed =
            accessControlAllowMethods corsMethodsAllowed
            *> accessControlAllowHeaders corsHeadersAllowed