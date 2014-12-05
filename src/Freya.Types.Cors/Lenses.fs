[<AutoOpen>]
module Freya.Types.Cors.Lenses

open Aether.Operators
open Freya.Types.Http

(* Aliases *)

type private ACAC = AccessControlAllowCredentials
type private ACAH = AccessControlAllowHeaders
type private ACAM = AccessControlAllowMethods
type private ACAO = AccessControlAllowOrigin
type private ACAE = AccessControlExposeHeaders
type private ACMA = AccessControlMaxAge
type private ACRH = AccessControlRequestHeaders
type private ACRM = AccessControlRequestMethod

(* Request Lenses *)

[<RequireQualifiedAccess>]
module Request =

    (* Request Header Lenses *)

    [<RequireQualifiedAccess>]
    module Headers =

        let private headerPIso key tryParse format =
            Request.headersKey key <??> (tryParse, format)

        let accessControlRequestHeaders =
            headerPIso "Access-Control-Request-Headers" ACRH.TryParse ACRH.Format

        let accessControlRequestMethod =
            headerPIso "Access-Control-Request-Method" ACRM.TryParse ACRM.Format

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
            headerPIso "Access-Control-Allow-Credentials" ACAC.TryParse ACAC.Format

        let accessControlAllowHeaders =
            headerPIso "Access-Control-Allow-Headers" ACAH.TryParse ACAH.Format

        let accessControlAllowMethods =
            headerPIso "Access-Control-Allow-Methods" ACAM.TryParse ACAM.Format

        let accessControlAllowOrigin =
            headerPIso "Access-Control-Allow-Origin" ACAO.TryParse ACAO.Format

        let accessControlExposeHeaders =
            headerPIso "Access-Control-Expose-Headers" ACAE.TryParse ACAE.Format

        let accessControlMaxAge =
            headerPIso "Access-Control-Max-Age" ACMA.TryParse ACMA.Format