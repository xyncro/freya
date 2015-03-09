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

    static member Mapping =

        let originP =
            OriginListOrNull.Mapping.Parse |>> Origin

        let originF =
            function | Origin x -> OriginListOrNull.Mapping.Format x

        { Parse = originP
          Format = originF }

    static member Format =
        Formatting.format Origin.Mapping.Format

    static member Parse =
        Parsing.parse Origin.Mapping.Parse

    static member TryParse =
        Parsing.tryParse Origin.Mapping.Parse

    override x.ToString () =
        Origin.Format x

and OriginListOrNull =
    | Origins of SerializedOrigin list
    | Null

    static member Mapping =

        let originListOrNullP =
            choice [
                attempt (sepBy1 SerializedOrigin.Mapping.Parse spaceP) |>> Origins
                skipString "null" >>% Null ]

        let originListOrNullF =
            function | Origins x -> join SerializedOrigin.Mapping.Format spaceF x
                     | Null -> append "null"

        { Parse = originListOrNullP
          Format = originListOrNullF }

and SerializedOrigin =
    | SerializedOrigin of Scheme * Host * Port option

    static member Mapping =

        let serializedOriginP =
            Scheme.Mapping.Parse .>> skipString "://" 
            .>>. Host.Mapping.Parse
            .>>. opt Port.Mapping.Parse
            |>> fun ((scheme, host), port) ->
                 Scheme.Mapping.Parse .>> skipString "://" 
            .>>. Host.Mapping.Parse
            .>>. opt Port.Mapping.Parse
             |>> fun ((scheme, host), port) ->
                SerializedOrigin (scheme, host, port)

        let serializedOriginF =
            function | SerializedOrigin (s, h, p) ->
                            let formatters =
                                [ Scheme.Mapping.Format s
                                  append "://"
                                  Host.Mapping.Format h
                                  (function | Some p -> Port.Mapping.Format p | _ -> id) p ]
                                  (function | Some p -> Port.Mapping.Format p
                                            | _ -> id) p ]

                            fun b -> List.fold (|>) b formatters

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

    static member Mapping =

        let accessControlAllowOriginP =
            choice [
                attempt OriginListOrNull.Mapping.Parse |>> (Origins >> AccessControlAllowOrigin)
                skipChar '*' >>% AccessControlAllowOrigin (Any) ]

        let accessControlAllowOriginF =
            function | AccessControlAllowOrigin (Origins x) -> OriginListOrNull.Mapping.Format x
                     | AccessControlAllowOrigin (Any) -> append "*"

        { Parse = accessControlAllowOriginP
          Format = accessControlAllowOriginF }

    static member Format =
        Formatting.format AccessControlAllowOrigin.Mapping.Format

    static member Parse =
        Parsing.parse AccessControlAllowOrigin.Mapping.Parse

    static member TryParse =
        Parsing.tryParse AccessControlAllowOrigin.Mapping.Parse

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

    static member Mapping =

        let accessControlAllowCredentialsP =
            skipString "true" >>% AccessControlAllowCredentials

        let accessControlAllowCredentialsF =
            function | AccessControlAllowCredentials -> append "true"

        { Parse = accessControlAllowCredentialsP
          Format = accessControlAllowCredentialsF }

    static member Format =
        Formatting.format AccessControlAllowCredentials.Mapping.Format

    static member Parse =
        Parsing.parse AccessControlAllowCredentials.Mapping.Parse

    static member TryParse =
        Parsing.tryParse AccessControlAllowCredentials.Mapping.Parse

    override x.ToString () =
        AccessControlAllowCredentials.Format x

(* Access-Control-Expose-Headers

   Taken from W3C Recommendation on CORS, Section 5.3 Access-Control-Expose-Headers
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlExposeHeaders =
    | AccessControlExposeHeaders of string list

    static member Mapping =

        let accessControlExposeHeadersP =
            infixP tokenP commaP |>> AccessControlExposeHeaders

        let accessControlExposeHeadersF =
            function | AccessControlExposeHeaders x -> join append commaF x

        { Parse = accessControlExposeHeadersP
          Format = accessControlExposeHeadersF }

    static member Format =
        Formatting.format AccessControlExposeHeaders.Mapping.Format

    static member Parse =
        Parsing.parse AccessControlExposeHeaders.Mapping.Parse

    static member TryParse =
        Parsing.tryParse AccessControlExposeHeaders.Mapping.Parse

    override x.ToString () =
        AccessControlExposeHeaders.Format x

(* Access-Control-Max-Age

   Taken from W3C Recommendation on CORS, Section 5.4 Access-Control-Max-Age
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlMaxAge =
    | AccessControlMaxAge of TimeSpan

    static member Mapping =

        let accessControlMaxAgeP =
            puint32 |>> (float >> TimeSpan.FromSeconds >> AccessControlMaxAge)

        let accessControlMaxAgeF =
            function | AccessControlMaxAge x -> append (string x.TotalSeconds)

        { Parse = accessControlMaxAgeP
          Format = accessControlMaxAgeF }

    static member Format =
        Formatting.format AccessControlMaxAge.Mapping.Format

    static member Parse =
        Parsing.parse AccessControlMaxAge.Mapping.Parse

    static member TryParse =
        Parsing.tryParse AccessControlMaxAge.Mapping.Parse

    override x.ToString () =
        AccessControlMaxAge.Format x

(* Access-Control-Allow-Methods

   Taken from W3C Recommendation on CORS, Section 5.5 Access-Control-Allow-Methods
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlAllowMethods =
    | AccessControlAllowMethods of Method list

    static member Mapping =

        let accessControlAllowMethodsP =
            infixP Method.Mapping.Parse commaP |>> AccessControlAllowMethods

        let accessControlAllowMethodsF =
            function | AccessControlAllowMethods x -> join Method.Mapping.Format commaF x

        { Parse = accessControlAllowMethodsP
          Format = accessControlAllowMethodsF }

    static member Format =
        Formatting.format AccessControlAllowMethods.Mapping.Format

    static member Parse =
        Parsing.parse AccessControlAllowMethods.Mapping.Parse

    static member TryParse =
        Parsing.tryParse AccessControlAllowMethods.Mapping.Parse

    override x.ToString () =
        AccessControlAllowMethods.Format x

(* Access-Control-Allow-Headers

   Taken from W3C Recommendation on CORS, Section 5.6 Access-Control-Allow-Headers
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlAllowHeaders =
    | AccessControlAllowHeaders of string list

    static member Mapping =

        let accessControlAllowHeadersP =
            infixP tokenP commaP |>> AccessControlAllowHeaders

        let accessControlAllowHeadersF =
            function | AccessControlAllowHeaders x -> join append commaF x

        { Parse = accessControlAllowHeadersP
          Format = accessControlAllowHeadersF }

    static member Format =
        Formatting.format AccessControlAllowHeaders.Mapping.Format

    static member Parse =
        Parsing.parse AccessControlAllowHeaders.Mapping.Parse

    static member TryParse =
        Parsing.tryParse AccessControlAllowHeaders.Mapping.Parse

    override x.ToString () =
        AccessControlAllowHeaders.Format x

(* Access-Control-Request-Method

   Taken from W3C Recommendation on CORS, Section 5.8 Access-Control-Request-Method
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlRequestMethod =
    | AccessControlRequestMethod of Method

    static member Mapping =

        let accessControlRequestMethodP =
            Method.Mapping.Parse |>> AccessControlRequestMethod

        let accessControlRequestMethodF =
            function | AccessControlRequestMethod x -> Method.Mapping.Format x

        { Parse = accessControlRequestMethodP
          Format = accessControlRequestMethodF }

    static member Format =
        Formatting.format AccessControlRequestMethod.Mapping.Format

    static member Parse =
        Parsing.parse AccessControlRequestMethod.Mapping.Parse

    static member TryParse =
        Parsing.tryParse AccessControlRequestMethod.Mapping.Parse

    override x.ToString () =
        AccessControlRequestMethod.Format x

(* Access-Control-Request-Headers

   Taken from W3C Recommendation on CORS, Section 5.9 Access-Control-Request-Headers
   See [http://www.w3.org/TR/2014/REC-cors-20140116/#syntax] *)

type AccessControlRequestHeaders =
    | AccessControlRequestHeaders of string list

    static member Mapping =

        let accessControlRequestHeadersP =
            infixP tokenP commaP |>> AccessControlRequestHeaders

        let accessControlRequestHeadersF =
            function | AccessControlRequestHeaders x -> join append commaF x

        { Parse = accessControlRequestHeadersP
          Format = accessControlRequestHeadersF }

    static member Format =
        Formatting.format AccessControlRequestHeaders.Mapping.Format

    static member Parse =
        Parsing.parse AccessControlRequestHeaders.Mapping.Parse

    static member TryParse =
        Parsing.tryParse AccessControlRequestHeaders.Mapping.Parse

    override x.ToString () =
        AccessControlRequestHeaders.Format x