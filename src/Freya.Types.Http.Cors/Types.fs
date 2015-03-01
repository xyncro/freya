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
module Freya.Types.Http.Cors.Types

open System
open Freya.Types
open Freya.Types.Formatting
open Freya.Types.Http
open Freya.Types.Parsing
open Freya.Types.Uri
open FParsec

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

    static member TypeMapping =

        let originP =
            OriginListOrNull.TypeMapping.Parse |>> Origin

        let originF =
            function | Origin x -> OriginListOrNull.TypeMapping.Format x

        { Parse = originP
          Format = originF }

    static member Format =
        Formatting.format Origin.TypeMapping.Format

    static member Parse =
        Parsing.parse Origin.TypeMapping.Parse

    static member TryParse =
        Parsing.tryParse Origin.TypeMapping.Parse

    override x.ToString () =
        Origin.Format x

and OriginListOrNull =
    | Origins of SerializedOrigin list
    | Null

    static member TypeMapping =

        let originListOrNullP =
            choice [
                attempt (sepBy1 SerializedOrigin.TypeMapping.Parse spaceP) |>> Origins
                skipString "null" >>% Null ]

        let originListOrNullF =
            function | Origins x -> join SerializedOrigin.TypeMapping.Format spaceF x
                     | Null -> append "null"

        { Parse = originListOrNullP
          Format = originListOrNullF }

and SerializedOrigin =
    | SerializedOrigin of Scheme * Host * Port option

    static member TypeMapping =

        let serializedOriginP =
            Scheme.TypeMapping.Parse .>> skipString "://" 
            .>>. Host.TypeMapping.Parse
            .>>. opt Port.TypeMapping.Parse
            |>> fun ((scheme, host), port) ->
                SerializedOrigin (scheme, host, port)

        let serializedOriginF =
            function | SerializedOrigin (s, h, p) ->
                            let formatters =
                                [ Scheme.TypeMapping.Format s
                                  append "://"
                                  Host.TypeMapping.Format h
                                  (function | Some p -> Port.TypeMapping.Format p | _ -> id) p ]

                            fun b -> List.fold (fun b f -> f b) b formatters

        { Parse = serializedOriginP
          Format = serializedOriginF }

(* W3C Recommendation on CORS

   Types, parsers and formatters implemented to mirror the specification of 
   CORS semantics as defined in W3C Recommendation on CORS (version dated 20140116).

   Taken from [http://www.w3.org/TR/2014/REC-cors-20140116] *)

(* Access-Control-Allow-Origin

   Taken from W3C Recommendation on CORS, Section 5.1 Access-Control-Allow-Origin
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlAllowOrigin =
    | AccessControlAllowOrigin of AccessControlAllowOriginRange

    static member TypeMapping =

        let accessControlAllowOriginP =
            choice [
                attempt OriginListOrNull.TypeMapping.Parse |>> (Origins >> AccessControlAllowOrigin)
                skipChar '*' >>% AccessControlAllowOrigin (Any) ]

        let accessControlAllowOriginF =
            function | AccessControlAllowOrigin (Origins x) -> OriginListOrNull.TypeMapping.Format x
                     | AccessControlAllowOrigin (Any) -> append "*"

        { Parse = accessControlAllowOriginP
          Format = accessControlAllowOriginF }

    static member Format =
        Formatting.format AccessControlAllowOrigin.TypeMapping.Format

    static member Parse =
        Parsing.parse AccessControlAllowOrigin.TypeMapping.Parse

    static member TryParse =
        Parsing.tryParse AccessControlAllowOrigin.TypeMapping.Parse

    override x.ToString () =
        AccessControlAllowOrigin.Format x

and AccessControlAllowOriginRange =
    | Origins of OriginListOrNull
    | Any

(* Access-Control-Allow-Credentials

   Taken from W3C Recommendation on CORS, Section 5.2 Access-Control-Allow-Credentials
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlAllowCredentials =
    | AccessControlAllowCredentials

    static member TypeMapping =

        let accessControlAllowCredentialsP =
            skipString "true" >>% AccessControlAllowCredentials

        let accessControlAllowCredentialsF =
            function | AccessControlAllowCredentials -> append "true"

        { Parse = accessControlAllowCredentialsP
          Format = accessControlAllowCredentialsF }

    static member Format =
        Formatting.format AccessControlAllowCredentials.TypeMapping.Format

    static member Parse =
        Parsing.parse AccessControlAllowCredentials.TypeMapping.Parse

    static member TryParse =
        Parsing.tryParse AccessControlAllowCredentials.TypeMapping.Parse

    override x.ToString () =
        AccessControlAllowCredentials.Format x

(* Access-Control-Expose-Headers

   Taken from W3C Recommendation on CORS, Section 5.3 Access-Control-Expose-Headers
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlExposeHeaders =
    | AccessControlExposeHeaders of string list

    static member TypeMapping =

        let accessControlExposeHeadersP =
            infixP tokenP commaP |>> AccessControlExposeHeaders

        let accessControlExposeHeadersF =
            function | AccessControlExposeHeaders x -> join append commaF x

        { Parse = accessControlExposeHeadersP
          Format = accessControlExposeHeadersF }

    static member Format =
        Formatting.format AccessControlExposeHeaders.TypeMapping.Format

    static member Parse =
        Parsing.parse AccessControlExposeHeaders.TypeMapping.Parse

    static member TryParse =
        Parsing.tryParse AccessControlExposeHeaders.TypeMapping.Parse

    override x.ToString () =
        AccessControlExposeHeaders.Format x

(* Access-Control-Max-Age

   Taken from W3C Recommendation on CORS, Section 5.4 Access-Control-Max-Age
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlMaxAge =
    | AccessControlMaxAge of TimeSpan

    static member TypeMapping =

        let accessControlMaxAgeP =
            puint32 |>> (float >> TimeSpan.FromSeconds >> AccessControlMaxAge)

        let accessControlMaxAgeF =
            function | AccessControlMaxAge x -> append (string x.TotalSeconds)

        { Parse = accessControlMaxAgeP
          Format = accessControlMaxAgeF }

    static member Format =
        Formatting.format AccessControlMaxAge.TypeMapping.Format

    static member Parse =
        Parsing.parse AccessControlMaxAge.TypeMapping.Parse

    static member TryParse =
        Parsing.tryParse AccessControlMaxAge.TypeMapping.Parse

    override x.ToString () =
        AccessControlMaxAge.Format x

(* Access-Control-Allow-Methods

   Taken from W3C Recommendation on CORS, Section 5.5 Access-Control-Allow-Methods
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlAllowMethods =
    | AccessControlAllowMethods of Method list

    static member TypeMapping =

        let accessControlAllowMethodsP =
            infixP Method.TypeMapping.Parse commaP |>> AccessControlAllowMethods

        let accessControlAllowMethodsF =
            function | AccessControlAllowMethods x -> join Method.TypeMapping.Format commaF x

        { Parse = accessControlAllowMethodsP
          Format = accessControlAllowMethodsF }

    static member Format =
        Formatting.format AccessControlAllowMethods.TypeMapping.Format

    static member Parse =
        Parsing.parse AccessControlAllowMethods.TypeMapping.Parse

    static member TryParse =
        Parsing.tryParse AccessControlAllowMethods.TypeMapping.Parse

    override x.ToString () =
        AccessControlAllowMethods.Format x

(* Access-Control-Allow-Headers

   Taken from W3C Recommendation on CORS, Section 5.6 Access-Control-Allow-Headers
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlAllowHeaders =
    | AccessControlAllowHeaders of string list

    static member TypeMapping =

        let accessControlAllowHeadersP =
            infixP tokenP commaP |>> AccessControlAllowHeaders

        let accessControlAllowHeadersF =
            function | AccessControlAllowHeaders x -> join append commaF x

        { Parse = accessControlAllowHeadersP
          Format = accessControlAllowHeadersF }

    static member Format =
        Formatting.format AccessControlAllowHeaders.TypeMapping.Format

    static member Parse =
        Parsing.parse AccessControlAllowHeaders.TypeMapping.Parse

    static member TryParse =
        Parsing.tryParse AccessControlAllowHeaders.TypeMapping.Parse

    override x.ToString () =
        AccessControlAllowHeaders.Format x

(* Access-Control-Request-Method

   Taken from W3C Recommendation on CORS, Section 5.8 Access-Control-Request-Method
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlRequestMethod =
    | AccessControlRequestMethod of Method

    static member TypeMapping =

        let accessControlRequestMethodP =
            Method.TypeMapping.Parse |>> AccessControlRequestMethod

        let accessControlRequestMethodF =
            function | AccessControlRequestMethod x -> Method.TypeMapping.Format x

        { Parse = accessControlRequestMethodP
          Format = accessControlRequestMethodF }

    static member Format =
        Formatting.format AccessControlRequestMethod.TypeMapping.Format

    static member Parse =
        Parsing.parse AccessControlRequestMethod.TypeMapping.Parse

    static member TryParse =
        Parsing.tryParse AccessControlRequestMethod.TypeMapping.Parse

    override x.ToString () =
        AccessControlRequestMethod.Format x

(* Access-Control-Request-Headers

   Taken from W3C Recommendation on CORS, Section 5.9 Access-Control-Request-Headers
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlRequestHeaders =
    | AccessControlRequestHeaders of string list

    static member TypeMapping =

        let accessControlRequestHeadersP =
            infixP tokenP commaP |>> AccessControlRequestHeaders

        let accessControlRequestHeadersF =
            function | AccessControlRequestHeaders x -> join append commaF x

        { Parse = accessControlRequestHeadersP
          Format = accessControlRequestHeadersF }

    static member Format =
        Formatting.format AccessControlRequestHeaders.TypeMapping.Format

    static member Parse =
        Parsing.parse AccessControlRequestHeaders.TypeMapping.Parse

    static member TryParse =
        Parsing.tryParse AccessControlRequestHeaders.TypeMapping.Parse

    override x.ToString () =
        AccessControlRequestHeaders.Format x