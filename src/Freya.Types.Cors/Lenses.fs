[<AutoOpen>]
module Freya.Types.Cors.Lenses

open Aether.Operators
open Freya.Types.Http

(* Request Lenses *)

[<RequireQualifiedAccess>]
module Request =

    (* Request Header Lenses *)

    [<RequireQualifiedAccess>]
    module Headers =

        let private headerPIso key tryParse format =
            Request.headersKey key <??> (tryParse, format)

        let accessControlRequestHeaders =
            Request.headersKey "Access-Control-Request-Headers"

        let accessControlRequestMethod =
            Request.headersKey "Access-Control-Request-Method"

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
            Request.headersKey "Access-Control-Allow-Headers"

        let accessControlAllowMethods =
            Request.headersKey "Access-Control-Allow-Methods"

        let accessControlAllowOrigin =
            headerPIso "Access-Control-Allow-Origin" AccessControlAllowOrigin.TryParse AccessControlAllowOrigin.Format

        let accessControlExposeHeaders =
            headerPIso "Access-Control-Expose-Headers" AccessControlExposeHeaders.TryParse AccessControlExposeHeaders.Format

        let accessControlMaxAge =
            Request.headersKey "Access-Control-Max-Age"