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

module Arachne.Http

open System
open System.ComponentModel
open Arachne.Formatting
open Arachne.Parsing
open Arachne.Uri
open FParsec

(* RFC 7230

   Types, parsers and formatters implemented to mirror the specification of 
   HTTP semantics as defined in RFC 7230.

   Taken from [http://tools.ietf.org/html/rfc7230] *)

(* Uniform Resource Identifiers

   Taken from RFC 7230, Section 2.7 Uniform Resource Identifiers
   See [http://tools.ietf.org/html/rfc7230#section-3.2.3] *)

type PartialUri =
    { Relative: RelativePart
      Query: Query option }

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member TypeMapping =

        let partialUriP =
            RelativePart.TypeMapping.Parse .>>. opt Query.TypeMapping.Parse
            |>> fun (relative, query) ->
                { Relative = relative
                  Query = query }

        let partialUriF =
            function | { PartialUri.Relative = r
                         Query = q } ->
                            let formatters =
                                [ RelativePart.TypeMapping.Format r
                                  (function | Some q -> Query.TypeMapping.Format q | _ -> id) q ]

                            fun b -> List.fold (|>) b formatters

        { Parse = partialUriP
          Format = partialUriF }

    static member Format =
        Formatting.format PartialUri.TypeMapping.Format

    static member Parse =
        Parsing.parse PartialUri.TypeMapping.Parse

    static member TryParse =
        Parsing.tryParse PartialUri.TypeMapping.Parse

    override x.ToString () =
        PartialUri.Format x

(* Whitespace

   Taken from RFC 7230, Section 3.2.3. Whitespace
   See [http://tools.ietf.org/html/rfc7230#section-3.2.3] *)

let internal owsP = 
    skipManySatisfy ((?>) Grammar.wsp)

//    let rwsP =
//        skipMany1Satisfy (fun c -> Set.contains c wsp)

let internal bwsP =
    owsP

(* Field Value Components

   Taken from RFC 7230, Section 3.2.6. Field Value Components
   See [http://tools.ietf.org/html/rfc7230#section-3.2.6] *)

let internal tchar = 
    Set.unionMany [ 
        set [ '!'; '#'; '$'; '%'; '&'; '\''; '*'
              '+'; '-'; '.'; '^'; '_'; '`'; '|'; '~' ]
        Grammar.alpha
        Grammar.digit ]

let internal tokenP = 
    many1Satisfy ((?>) tchar)

let internal obsText =
    charRange 0x80 0xff

let internal qdtext =
    Set.unionMany [
        set [ Grammar.htab; Grammar.sp; char 0x21 ]
        charRange 0x23 0x5b
        charRange 0x5d 0x7e
        obsText ]

//let ctext =
//    Set.unionMany [
//        set [ htab; sp ]
//        charRange 0x21 0x27
//        charRange 0x2a 0x5b
//        charRange 0x5d 0x7e
//        obsText ]

let internal quotedPairChars =
    Set.unionMany [
        set [ Grammar.htab; Grammar.sp ]
        Grammar.vchar
        obsText ]

let internal quotedPairP : Parser<char, unit> =
        skipChar '\\' 
    >>. satisfy ((?>) quotedPairChars)

let internal quotedStringP : Parser<string, unit> =
        skipChar Grammar.dquote 
    >>. many (quotedPairP <|> satisfy ((?>) qdtext)) |>> (fun x -> string (String (List.toArray x)))
    .>> skipChar Grammar.dquote

(* ABNF List Extension: #rule

   Taken from RFC 7230, Section 7. ABNF List Extension: #rule
   [http://tools.ietf.org/html/rfc7230#section-7] *)

let private infixHeadP p s =
    (attempt p |>> Some) <|> (s >>% None)

let private infixTailP p s =
    many (owsP >>? s >>? owsP >>? opt p)

(* Note:
   The infix and prefix parsers are designed to convey as accurately as possible the 
   meaning of the ABNF #rule extension including the laxity of specification for backward 
   compatibility. Whether they are a perfectly true representation is open to debate, 
   but they should perform sensibly under normal conditions. *)

let internal infixP p s = 
    infixHeadP p s .>>. infixTailP p s .>> owsP |>> fun (x, xs) -> x :: xs |> List.choose id

let internal infix1P p s =
    notEmpty (infixP p s)

let internal prefixP p s =
    many (owsP >>? s >>? owsP >>? p)

(* HTTP Version

   Taken from RFC 7230, Section 3.1 Request Line
   See [http://tools.ietf.org/html/rfc7230#section-3.1] *)

type HttpVersion =
    | HTTP of float 
    | Custom of string

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member TypeMapping =

        let httpVersionP =
            choice [
                skipString "HTTP/1.0" >>% HttpVersion.HTTP 1.0
                skipString "HTTP/1.1" >>% HttpVersion.HTTP 1.1
                restOfLine false |>> HttpVersion.Custom ]

        let httpVersionF =
            function | HttpVersion.HTTP x -> appendf1 "HTTP/{0:G4}" x 
                     | HttpVersion.Custom x -> append x

        { Parse = httpVersionP
          Format = httpVersionF }

    static member Format =
        Formatting.format HttpVersion.TypeMapping.Format

    static member Parse =
        Parsing.parse HttpVersion.TypeMapping.Parse

    static member TryParse =
        Parsing.tryParse HttpVersion.TypeMapping.Parse

    override x.ToString () =
        HttpVersion.Format x

(* Content-Length

   Taken from RFC 7230, Section 3.3.2 Content-Length
   See [http://tools.ietf.org/html/rfc7230#section-3.3.2] *)

type ContentLength =
    | ContentLength of int

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member TypeMapping =

        let contentLengthP =
            puint32 |>> (int >> ContentLength)

        let contentLengthF =
            function | ContentLength x -> append (string x)

        { Parse = contentLengthP
          Format = contentLengthF }

    static member Format =
        Formatting.format ContentLength.TypeMapping.Format

    static member Parse =
        Parsing.parse ContentLength.TypeMapping.Parse

    static member TryParse =
        Parsing.tryParse ContentLength.TypeMapping.Parse

    override x.ToString () =
        ContentLength.Format x

(* Host

   Taken from RFC 7230, Section 5.4 Host
   See [http://tools.ietf.org/html/rfc7230#section-5.4] *)

type Host =
    | Host of Uri.Host * Port option

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member TypeMapping =

        let hostP =
            Uri.Host.TypeMapping.Parse .>>. opt Port.TypeMapping.Parse |>> Host

        let hostF =
            function | Host (h, p) ->
                        let formatters =
                            [ Uri.Host.TypeMapping.Format h
                              (function | Some p -> Port.TypeMapping.Format p | _ -> id) p ]

                        fun b -> List.fold (|>) b formatters

        { Parse = hostP
          Format = hostF }

    static member Format =
        Formatting.format Host.TypeMapping.Format

    static member Parse =
        Parsing.parse Host.TypeMapping.Parse

    static member TryParse =
        Parsing.tryParse Host.TypeMapping.Parse

    override x.ToString () =
        Host.Format x

(* Connection

   Taken from RFC 7230, Section 6.1 Connection
   See [http://tools.ietf.org/html/rfc7230#section-6.1] *)

type Connection =
    | Connection of ConnectionOption list

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member TypeMapping =

        let connectionP =
            infix1P tokenP commaP |>> (List.map ConnectionOption >> Connection)

        let connectionF =
            function | Connection x -> join (fun (ConnectionOption x) -> append x) commaF x

        { Parse = connectionP
          Format = connectionF }

    static member Format =
        Formatting.format Connection.TypeMapping.Format

    static member Parse =
        Parsing.parse Connection.TypeMapping.Parse

    static member TryParse =
        Parsing.tryParse Connection.TypeMapping.Parse

    override x.ToString () =
        Connection.Format x

and ConnectionOption =
    | ConnectionOption of string

(* RFC 7231

   Types, parsers and formatters implemented to mirror the semantics of
   HTTP as defined in RFC 7231.
   
   See [http://tools.ietf.org/html/rfc7231] *)

(* Media-Type

   Includes the common definition of parameter as defined within this
   section, but applicable to multiple later types.

   Taken from RFC 7231, Section 3.1.1.1 Media-Type
   [http://tools.ietf.org/html/rfc7231#section-3.1.1.1] *)

type MediaType =
    | MediaType of Type * SubType * Parameters

    static member TypeMapping =

        let mediaTypeP =
            tokenP .>> slashP .>>. tokenP .>>. Parameters.TypeMapping.Parse
            |>> (fun ((x, y), p) -> MediaType (Type x, SubType y, p))

        let mediaTypeF =
            function | MediaType (Type x, SubType y, p) -> 
                        appendf2 "{0}/{1}" x y >> Parameters.TypeMapping.Format p

        { Parse = mediaTypeP
          Format = mediaTypeF }

    static member Format =
        Formatting.format MediaType.TypeMapping.Format

    static member Parse =
        Parsing.parse MediaType.TypeMapping.Parse

    static member TryParse =
        Parsing.tryParse MediaType.TypeMapping.Parse

    override x.ToString () =
        MediaType.Format x

//    static member TypeLens : Lens<MediaType, Type> =
//        (fun (MediaType (x, _, _)) -> x), (fun x (MediaType (_, y, z)) -> MediaType (x, y, z))
//
//    static member SubTypeLens : Lens<MediaType, SubType> =
//        (fun (MediaType (_, y, _)) -> y), (fun y (MediaType (x, _, z)) -> MediaType (x, y, z))
//
//    static member ParametersLens : Lens<MediaType, Parameters> =
//        (fun (MediaType (_, _, z)) -> z), (fun z (MediaType (x, y, _)) -> MediaType (x, y, z))

and Parameters =
    | Parameters of Map<string, string>

    static member TypeMapping =

        let parameterP =
            tokenP .>> skipChar '=' .>>. (quotedStringP <|> tokenP)

        let parametersP =
            prefixP parameterP semicolonP |>> (Map.ofList >> Parameters)

        let pairF =
            (<||) (appendf2 "{0}={1}")

        let parametersF =
            function | Parameters (x: Map<string, string>) when Map.isEmpty x -> id
                     | Parameters (x) -> append ";" >> join pairF semicolonF (Map.toList x |> List.rev)

        { Parse = parametersP
          Format = parametersF }

and Type =
    | Type of string

and SubType =
    | SubType of string

(* Media-Type Presets *)

type MediaType with

    static member HTML =
        MediaType (Type "text", SubType "html", Parameters Map.empty)

    static member JSON =
        MediaType (Type "application", SubType "json", Parameters Map.empty)

    /// Convenience definition for "text/plain" without extra parameters
    static member Text =
        MediaType (Type "text", SubType "plain", Parameters Map.empty)

    /// Convenience definition for "application/xml" without extra parameters
    static member XML =
        MediaType (Type "application", SubType "xml", Parameters Map.empty)
