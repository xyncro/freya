[<AutoOpen>]
module Freya.Types.Cors.Types

#nowarn "60"

open FParsec
open Freya.Types
open Freya.Types.Http
open Freya.Types.Uri

(* RFC 6454

   Types, parsers and formatters implemented to mirror the specification of 
   Origin semantics as defined in RFC 6454.

   Taken from [http://tools.ietf.org/html/rfc6454] *)

type Origin =
    | Origin of OriginListOrNull

and OriginListOrNull =
    | Origins of SerializedOrigin list
    | Null

and SerializedOrigin =
    { Scheme: Scheme
      Host: Host
      Port: Port option }

(* Formatting *)

let private serializedOriginF =
    function | { Scheme = s
                 Host = h
                 Port = p } ->
                    let formatters =
                        [ schemeF s
                          hostF h
                          (function | Some p -> portF p | _ -> id) p ]

                    fun b -> List.fold (fun b f -> f b) b formatters

let private originListOrNullF =
    function | Origins x -> join serializedOriginF spaceF x
             | Null -> append "null"

let private originF =
    function | Origin x -> originListOrNullF x

(* Parsing *)

let private serializedOriginP =
    schemeP .>> skipString "://" .>>. hostP .>>. opt portP
    |>> fun ((scheme, host), port) ->
        { Scheme = scheme
          Host = host
          Port = port }

let private originListOrNullP =
    choice [
        attempt (sepBy1 serializedOriginP spaceP) |>> Origins
        skipString "null" >>% Null ]

let private originP =
    originListOrNullP |>> Origin

(* Augmentation *)

type Origin with

    static member Format =
        format originF

    static member Parse =
        parseExact originP

    static member TryParse =
        parseOption originP

    override x.ToString () =
        Origin.Format x

(* W3C Recommendation on CORS

   Types, parsers and formatters implemented to mirror the specification of 
   CORS semantics as defined in W3C Recommendation on CORS (version dated 20140116).

   Taken from [http://www.w3.org/TR/2014/REC-cors-20140116] *)

(* Access-Control-Allow-Origin

   Taken from W3C Recommendation on CORS, Section 5.1 Access-Control-Allow-Origin
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlAllowOrigin =
    | AccessControlAllowOrigin of AccessControlAllowOriginRange

and AccessControlAllowOriginRange =
    | Origins of OriginListOrNull
    | Any

let private accessControlAllowOriginF =
    function | AccessControlAllowOrigin (Origins x) -> originListOrNullF x
             | AccessControlAllowOrigin (Any) -> append "*"

let private accessControlAllowOriginP =
    choice [
        attempt (originListOrNullP) |>> (Origins >> AccessControlAllowOrigin)
        skipChar '*' >>% AccessControlAllowOrigin (Any) ]

type AccessControlAllowOrigin with

    static member Format =
        format accessControlAllowOriginF

    static member Parse =
        parseExact accessControlAllowOriginP

    static member TryParse =
        parseOption accessControlAllowOriginP

    override x.ToString () =
        AccessControlAllowOrigin.Format x

(* Access-Control-Allow-Credentials

   Taken from W3C Recommendation on CORS, Section 5.2 Access-Control-Allow-Credentials
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlAllowCredentials =
    | AccessControlAllowCredentials

let private accessControlAllowCredentialsF =
    function | AccessControlAllowCredentials -> append "true"

let private accessControlAllowCredentialsP =
    skipString "true" >>% AccessControlAllowCredentials

type AccessControlAllowCredentials with

    static member Format =
        format accessControlAllowCredentialsF

    static member Parse =
        parseExact accessControlAllowCredentialsP

    static member TryParse =
        parseOption accessControlAllowCredentialsP

    override x.ToString () =
        AccessControlAllowCredentials.Format x

(* Access-Control-Expose-Headers

   Taken from W3C Recommendation on CORS, Section 5.3 Access-Control-Expose-Headers
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlExposeHeaders =
    | AccessControlExposeHeaders of string list

let private accessControlExposeHeadersF =
    function | AccessControlExposeHeaders x -> join append commaF x

let private accessControlExposeHeadersP =
    infixP tokenP commaP |>> AccessControlExposeHeaders

type AccessControlExposeHeaders with

    static member Format =
        format accessControlExposeHeadersF

    static member Parse =
        parseExact accessControlExposeHeadersP

    static member TryParse =
        parseOption accessControlExposeHeadersP

    override x.ToString () =
        AccessControlExposeHeaders.Format x