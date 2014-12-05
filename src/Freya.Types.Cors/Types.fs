[<AutoOpen>]
module Freya.Types.Cors.Types

#nowarn "60"

open System
open FParsec
open Freya.Types
open Freya.Types.Http
open Freya.Types.Uri

(* RFC 6454

   Types, parsers and formatters implemented to mirror the specification of 
   Origin semantics as defined in RFC 6454.

   Taken from [http://tools.ietf.org/html/rfc6454] *)

(* Origin

   Taken from RFC 6454, Section 7 Origin
   See [http://tools.ietf.org/html/rfc6454#section-7]

   Also described as part of the W3C Recommendation on CORS, Section 5.7
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

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
                          append "://"
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

(* Access-Control-Max-Age

   Taken from W3C Recommendation on CORS, Section 5.4 Access-Control-Max-Age
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlMaxAge =
    | AccessControlMaxAge of TimeSpan

let private accessControlMaxAgeF =
    function | AccessControlMaxAge x -> append (string x.TotalSeconds)

let private accessControlMaxAgeP =
    puint32 |>> (float >> TimeSpan.FromSeconds >> AccessControlMaxAge)

type AccessControlMaxAge with

    static member Format =
        format accessControlMaxAgeF

    static member Parse =
        parseExact accessControlMaxAgeP

    static member TryParse =
        parseOption accessControlMaxAgeP

    override x.ToString () =
        AccessControlMaxAge.Format x

(* Access-Control-Allow-Methods

   Taken from W3C Recommendation on CORS, Section 5.5 Access-Control-Allow-Methods
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlAllowMethods =
    | AccessControlAllowMethods of Method list

let private accessControlAllowMethodsF =
    function | AccessControlAllowMethods x -> join methodF commaF x

let private accessControlAllowMethodsP =
    infixP methodP commaP |>> AccessControlAllowMethods

type AccessControlAllowMethods with

    static member Format =
        format accessControlAllowMethodsF

    static member Parse =
        parseExact accessControlAllowMethodsP

    static member TryParse =
        parseOption accessControlAllowMethodsP

    override x.ToString () =
        AccessControlAllowMethods.Format x

(* Access-Control-Allow-Headers

   Taken from W3C Recommendation on CORS, Section 5.6 Access-Control-Allow-Headers
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlAllowHeaders =
    | AccessControlAllowHeaders of string list

let private accessControlAllowHeadersF =
    function | AccessControlAllowHeaders x -> join append commaF x

let private accessControlAllowHeadersP =
    infixP tokenP commaP |>> AccessControlAllowHeaders

type AccessControlAllowHeaders with

    static member Format =
        format accessControlAllowHeadersF

    static member Parse =
        parseExact accessControlAllowHeadersP

    static member TryParse =
        parseOption accessControlAllowHeadersP

    override x.ToString () =
        AccessControlAllowHeaders.Format x

(* Access-Control-Request-Method

   Taken from W3C Recommendation on CORS, Section 5.8 Access-Control-Request-Method
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlRequestMethod =
    | AccessControlRequestMethod of Method

let private accessControlRequestMethodF =
    function | AccessControlRequestMethod x -> methodF x

let private accessControlRequestMethodP =
    methodP |>> AccessControlRequestMethod

type AccessControlRequestMethod with

    static member Format =
        format accessControlRequestMethodF

    static member Parse =
        parseExact accessControlRequestMethodP

    static member TryParse =
        parseOption accessControlRequestMethodP

    override x.ToString () =
        AccessControlRequestMethod.Format x

(* Access-Control-Request-Headers

   Taken from W3C Recommendation on CORS, Section 5.9 Access-Control-Request-Headers
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlRequestHeaders =
    | AccessControlRequestHeaders of string list

let private accessControlRequestHeadersF =
    function | AccessControlRequestHeaders x -> join append commaF x

let private accessControlRequestHeadersP =
    infixP tokenP commaP |>> AccessControlRequestHeaders

type AccessControlRequestHeaders with

    static member Format =
        format accessControlRequestHeadersF

    static member Parse =
        parseExact accessControlRequestHeadersP

    static member TryParse =
        parseOption accessControlRequestHeadersP

    override x.ToString () =
        AccessControlRequestHeaders.Format x