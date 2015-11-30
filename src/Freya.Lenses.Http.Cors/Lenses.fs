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

open System
open Aether.Operators
open Arachne.Http.Cors
open Freya.Core
open Freya.Lenses.Http

(* Obsolete

   Backwards compatibility shims to make the 2.x-> 3.x transition
   less painful, providing functionally equivalent options where possible.

   To be removed for 4.x releases. *)

let private option_ =
    id, fun x -> Some x

(* Request Lenses *)

[<RequireQualifiedAccess>]
module Request =

    (* Request Header Lenses *)

    [<RequireQualifiedAccess>]
    module Headers =

        let private value_ key e =
                Request.header_ key
            >-> Option.mapEpimorphism e

        let accessControlRequestHeaders_ =
            value_
                "Access-Control-Request-Headers"
                (AccessControlRequestHeaders.TryParse, AccessControlRequestHeaders.Format)

        let accessControlRequestMethod_ =
            value_
                "Access-Control-Request-Method"
                (AccessControlRequestMethod.TryParse, AccessControlRequestMethod.Format)

        let origin_ =
            value_
                "Origin"
                (Origin.TryParse, Origin.Format)

        (* Obsolete

           Backwards compatibility shims to make the 2.x-> 3.x transition
           less painful, providing functionally equivalent options where possible.
//
           To be removed for 4.x releases. *)

        [<Obsolete ("Use Request.Headers.accessControlRequestHeaders_ instead.")>]
        let AccessControlRequestHeaders_ =
                accessControlRequestHeaders_
            >-> option_

        [<Obsolete ("Use Request.Headers.accessControlRequestMethod_ instead.")>]
        let AccessControlRequestMethod_ =
                accessControlRequestMethod_
            >-> option_

        [<Obsolete ("Use Request.Headers.origin_ instead.")>]
        let Origin_ =
                origin_
            >-> option_

(* Response Lenses *)

[<RequireQualifiedAccess>]
module Response =

    (* Response Header Lenses *)

    [<RequireQualifiedAccess>]
    module Headers =

        let private value_ key e =
                Response.header_ key
            >-> Option.mapEpimorphism e

        let accessControlAllowCredentials_ =
            value_
                "Access-Control-Allow-Credentials"
                (AccessControlAllowCredentials.TryParse, AccessControlAllowCredentials.Format)

        let accessControlAllowHeaders_ =
            value_
                "Access-Control-Allow-Headers"
                (AccessControlAllowHeaders.TryParse, AccessControlAllowHeaders.Format)

        let accessControlAllowMethods_ =
            value_
                "Access-Control-Allow-Methods"
                (AccessControlAllowMethods.TryParse, AccessControlAllowMethods.Format)

        let accessControlAllowOrigin_ =
            value_
                "Access-Control-Allow-Origin"
                (AccessControlAllowOrigin.TryParse, AccessControlAllowOrigin.Format)

        let accessControlExposeHeaders_ =
            value_
                "Access-Control-Expose-Headers"
                (AccessControlExposeHeaders.TryParse, AccessControlExposeHeaders.Format)

        let accessControlMaxAge_ =
            value_
                "Access-Control-Max-Age"
                (AccessControlMaxAge.TryParse, AccessControlMaxAge.Format)

        (* Obsolete

           Backwards compatibility shims to make the 2.x-> 3.x transition
           less painful, providing functionally equivalent options where possible.
//
           To be removed for 4.x releases. *)

        [<Obsolete ("Use Response.Headers.accessControlAllowCredentials_ instead.")>]
        let AccessControlAllowCredentials_ =
                accessControlAllowCredentials_
            >-> option_

        [<Obsolete ("Use Response.Headers.accessControlAllowHeaders_ instead.")>]
        let AccessControlAllowHeaders_ =
                accessControlAllowHeaders_
            >-> option_

        [<Obsolete ("Use Response.Headers.accessControlAllowMethods_ instead.")>]
        let AccessControlAllowMethods_ =
                accessControlAllowMethods_
            >-> option_

        [<Obsolete ("Use Response.Headers.accessControlAllowOrigin_ instead.")>]
        let AccessControlAllowOrigin_ =
                accessControlAllowOrigin_
            >-> option_

        [<Obsolete ("Use Response.Headers.accessControlExposeHeaders_ instead.")>]
        let AccessControlExposeHeaders_ =
                accessControlExposeHeaders_
            >-> option_

        [<Obsolete ("Use Response.Headers.accessControlMaxAge_ instead.")>]
        let AccessControlMaxAge_ =
                accessControlMaxAge_
            >-> option_