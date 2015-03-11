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
module Freya.Types.Http.Types

open System
open System.ComponentModel
open System.Globalization
open System.Runtime.CompilerServices
open Freya.Types
//open Freya.Types.Formatting
open Freya.Types.Language
//open Freya.Types.Parsing
open Freya.Types.Uri
open FParsec

(* Internals *)

[<assembly:InternalsVisibleTo ("Freya.Types.Http.Cors")>]
do ()

(* RFC 7230

   Types, parsers and formatters implemented to mirror the specification of 
   HTTP semantics as defined in RFC 7230.

   Taken from [http://tools.ietf.org/html/rfc7230] *)

(* Uniform Resource Identifiers

   Taken from RFC 7230, Section 2.7 Uniform Resource Identifiers
   See [http://tools.ietf.org/html/rfc7230#section-3.2.3] *)

type PartialUri =
    | PartialUri of RelativePart * Query option

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let partialUriP =
            RelativePart.Mapping.Parse .>>. opt Query.Mapping.Parse
            |>> PartialUri

        let partialUriF =
            function | PartialUri (r, q) ->
                        let formatters =
                            [ RelativePart.Mapping.Format r
                              (function | Some q -> Query.Mapping.Format q | _ -> id) q ]

                        fun b -> List.fold (|>) b formatters

        { Parse = partialUriP
          Format = partialUriF }

    static member Format =
        Formatting.format PartialUri.Mapping.Format

    static member Parse =
        Parsing.parse PartialUri.Mapping.Parse

    static member TryParse =
        Parsing.tryParse PartialUri.Mapping.Parse

    override x.ToString () =
        PartialUri.Format x

(* Whitespace

   Taken from RFC 7230, Section 3.2.3. Whitespace
   See [http://tools.ietf.org/html/rfc7230#section-3.2.3] *)

let internal owsP = 
    skipManySatisfy (int >> Grammar.wsp)

//    let rwsP =
//        skipMany1Satisfy (fun c -> Set.contains c wsp)

let internal bwsP =
    owsP

(* Field Value Components

   Taken from RFC 7230, Section 3.2.6. Field Value Components
   See [http://tools.ietf.org/html/rfc7230#section-3.2.6] *)

let internal tchar i =
        (Grammar.alpha i)
     || (Grammar.digit i)
     || (i = 0x21) // !
     || (i >= 0x23 && i <= 0x26) // # $ % &
     || (i = 0x5c) // \
     || (i = 0x2a) // *
     || (i = 0x2b) // +
     || (i = 0x2d) // -
     || (i = 0x2e) // .
     || (i = 0x5e) // ^
     || (i = 0x5f) // _
     || (i = 0x60) // `
     || (i = 0x7c) // |
     || (i = 0x7e) // ~

let internal obstext i =
        (i >= 0x80 && i <= 0xff)

let internal qdtext i =
        (Grammar.htab i)
     || (Grammar.sp i)
     || (i = 0x21)
     || (i >= 0x23 && i <= 0x5b)
     || (i >= 0x5d && i <= 0x7e)
     || (obstext i)

let internal quotedPairChar i =
        (Grammar.htab i)
     || (Grammar.sp i)
     || (Grammar.vchar i)
     || (obstext i)

let internal tokenP = 
    many1Satisfy (int >> tchar)

//let ctext =
//    Set.unionMany [
//        set [ htab; sp ]
//        charRange 0x21 0x27
//        charRange 0x2a 0x5b
//        charRange 0x5d 0x7e
//        obsText ]

let internal quotedPairP : Parser<char, unit> =
        skipChar '\\' 
    >>. satisfy (int >> quotedPairChar)

let internal quotedStringP : Parser<string, unit> =
        skipSatisfy (int >> Grammar.dquote)
    >>. many (quotedPairP <|> satisfy (int >> qdtext)) |>> (fun x -> string (System.String (List.toArray x)))
    .>> skipSatisfy (int >> Grammar.dquote)

(* ABNF List Extension: #rule

   Taken from RFC 7230, Section 7. ABNF List Extension: #rule
   [http://tools.ietf.org/html/rfc7230#section-7] *)

let internal infixHeadP p s =
    (attempt p |>> Some) <|> (s >>% None)

let internal infixTailP p s =
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
    static member Mapping =

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
        Formatting.format HttpVersion.Mapping.Format

    static member Parse =
        Parsing.parse HttpVersion.Mapping.Parse

    static member TryParse =
        Parsing.tryParse HttpVersion.Mapping.Parse

    override x.ToString () =
        HttpVersion.Format x

(* Content-Length

   Taken from RFC 7230, Section 3.3.2 Content-Length
   See [http://tools.ietf.org/html/rfc7230#section-3.3.2] *)

type ContentLength =
    | ContentLength of int

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let contentLengthP =
            puint32 |>> (int >> ContentLength)

        let contentLengthF =
            function | ContentLength x -> append (string x)

        { Parse = contentLengthP
          Format = contentLengthF }

    static member Format =
        Formatting.format ContentLength.Mapping.Format

    static member Parse =
        Parsing.parse ContentLength.Mapping.Parse

    static member TryParse =
        Parsing.tryParse ContentLength.Mapping.Parse

    override x.ToString () =
        ContentLength.Format x

(* Host

   Taken from RFC 7230, Section 5.4 Host
   See [http://tools.ietf.org/html/rfc7230#section-5.4] *)

type Host =
    | Host of Uri.Host * Port option

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let hostP =
            Uri.Host.Mapping.Parse .>>. opt Port.Mapping.Parse |>> Host

        let hostF =
            function | Host (h, p) ->
                        let formatters =
                            [ Uri.Host.Mapping.Format h
                              (function | Some p -> Port.Mapping.Format p
                                        | _ -> id) p ]

                        fun b -> List.fold (|>) b formatters

        { Parse = hostP
          Format = hostF }

    static member Format =
        Formatting.format Host.Mapping.Format

    static member Parse =
        Parsing.parse Host.Mapping.Parse

    static member TryParse =
        Parsing.tryParse Host.Mapping.Parse

    override x.ToString () =
        Host.Format x

(* Connection

   Taken from RFC 7230, Section 6.1 Connection
   See [http://tools.ietf.org/html/rfc7230#section-6.1] *)

type Connection =
    | Connection of ConnectionOption list

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let connectionP =
            infix1P tokenP commaP |>> (List.map ConnectionOption >> Connection)

        let connectionF =
            function | Connection x -> join (fun (ConnectionOption x) -> append x) commaF x

        { Parse = connectionP
          Format = connectionF }

    static member Format =
        Formatting.format Connection.Mapping.Format

    static member Parse =
        Parsing.parse Connection.Mapping.Parse

    static member TryParse =
        Parsing.tryParse Connection.Mapping.Parse

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

    static member Mapping =

        let mediaTypeP =
            tokenP .>> slashP .>>. tokenP .>>. Parameters.Mapping.Parse
            |>> (fun ((x, y), p) -> MediaType (Type x, SubType y, p))

        let mediaTypeF =
            function | MediaType (Type x, SubType y, p) -> 
                        appendf2 "{0}/{1}" x y >> Parameters.Mapping.Format p

        { Parse = mediaTypeP
          Format = mediaTypeF }

    (* Lenses *)

    static member TypeLens =
        (fun (MediaType (x, _, _)) -> x), (fun x (MediaType (_, y, z)) -> MediaType (x, y, z))

    static member SubTypeLens =
        (fun (MediaType (_, y, _)) -> y), (fun y (MediaType (x, _, z)) -> MediaType (x, y, z))

    static member ParametersLens =
        (fun (MediaType (_, _, z)) -> z), (fun z (MediaType (x, y, _)) -> MediaType (x, y, z))

    (* Common *)

    static member Format =
        Formatting.format MediaType.Mapping.Format

    static member Parse =
        Parsing.parse MediaType.Mapping.Parse

    static member TryParse =
        Parsing.tryParse MediaType.Mapping.Parse

    override x.ToString () =
        MediaType.Format x


and Parameters =
    | Parameters of Map<string, string>

    static member Mapping =

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

    static member ParametersIso =
        (fun (Parameters x) -> x), (fun x -> Parameters x)

and Type =
    | Type of string

and SubType =
    | SubType of string

(* Media-Type Presets *)

type MediaType with

    static member Css =
        MediaType (Type "text", SubType "css", Parameters Map.empty)

    static member Html =
        MediaType (Type "text", SubType "html", Parameters Map.empty)

    static member JavaScript =
        MediaType (Type "application", SubType "javascript", Parameters Map.empty)

    static member Json =
        MediaType (Type "application", SubType "json", Parameters Map.empty)

    /// Convenience definition for "text/plain" without extra parameters
    static member Text =
        MediaType (Type "text", SubType "plain", Parameters Map.empty)

    /// Convenience definition for "application/xml" without extra parameters
    static member Xml =
        MediaType (Type "application", SubType "xml", Parameters Map.empty)

(* Content-Type

    Taken from RFC 7231, Section 3.1.1.5 Content-Type
    [http://tools.ietf.org/html/rfc7231#section-3.1.1.5] *)

type ContentType =
    | ContentType of MediaType

    static member Mapping =

        let contentTypeP =
            MediaType.Mapping.Parse |>> ContentType

        let contentTypeF =
            function | ContentType x -> MediaType.Mapping.Format x

        { Parse = contentTypeP
          Format = contentTypeF }

    (* Lenses *)

    static member MediaTypeIso =
        (fun (ContentType x) -> x), (fun x -> ContentType x)

    (* Common *)

    static member Format =
        Formatting.format ContentType.Mapping.Format

    static member Parse =
        Parsing.parse ContentType.Mapping.Parse

    static member TryParse =
        Parsing.tryParse ContentType.Mapping.Parse

    override x.ToString () =
        ContentType.Format x

(* Content-Encoding

   Taken from RFC 7231, Section 3.1.2.2 Content-Encoding
   [http://tools.ietf.org/html/rfc7231#section-3.1.2.2] *)

type ContentEncoding =
    | ContentEncoding of ContentCoding list

    static member Mapping =

        let contentEncodingP =
            infix1P tokenP commaP |>> (List.map ContentCoding >> ContentEncoding)

        let contentEncodingF =
            function | ContentEncoding x -> join (fun (ContentCoding x) -> append x) commaF x

        { Parse = contentEncodingP
          Format = contentEncodingF }

    static member Format =
        Formatting.format ContentEncoding.Mapping.Format

    static member Parse =
        Parsing.parse ContentEncoding.Mapping.Parse

    static member TryParse =
        Parsing.tryParse ContentEncoding.Mapping.Parse

    override x.ToString () =
        ContentEncoding.Format x

and ContentCoding =
    | ContentCoding of string

(* Content-Encoding Presets *)

type ContentCoding with

    /// Convenience definition for "deflate"
    static member Compress =
        ContentCoding "compress"

    /// Convenience definition for "deflate"
    static member Deflate =
        ContentCoding "deflate"

    /// Convenience definition for "gzip"
    static member GZip =
        ContentCoding "gzip"

(* Content-Language

   Taken from RFC 7231, Section 3.1.3.2 Content-Language
   [http://tools.ietf.org/html/rfc7231#section-3.1.3.2] *)

type ContentLanguage =
    | ContentLanguage of LanguageTag list

    static member Mapping =

        let contentLanguageP =
            infix1P LanguageTag.Mapping.Parse commaP |>> ContentLanguage

        let contentLanguageF =
            function | ContentLanguage xs -> join LanguageTag.Mapping.Format commaF xs

        { Parse = contentLanguageP
          Format = contentLanguageF }

    static member Format =
        Formatting.format ContentLanguage.Mapping.Format

    static member Parse =
        Parsing.parse ContentLanguage.Mapping.Parse

    static member TryParse =
        Parsing.tryParse ContentLanguage.Mapping.Parse

    override x.ToString () =
        ContentLanguage.Format x

(* Content-Location

   Taken from RFC 7231, Section 3.1.4.2 Content-Location
   [http://tools.ietf.org/html/rfc7231#section-3.1.4.2] *)

type ContentLocation =
    | ContentLocation of ContentLocationUri

    static member Mapping =

        let contentLocationP =
            choice [
                attempt AbsoluteUri.Mapping.Parse |>> (Absolute >> ContentLocation)
                PartialUri.Mapping.Parse |>> (Partial >> ContentLocation) ]

        let contentLocationF =
            function | ContentLocation (Absolute x) -> AbsoluteUri.Mapping.Format x
                     | ContentLocation (Partial x) -> PartialUri.Mapping.Format x

        { Parse = contentLocationP
          Format = contentLocationF }

    static member Format =
        Formatting.format ContentLocation.Mapping.Format

    static member Parse =
        Parsing.parse ContentLocation.Mapping.Parse

    static member TryParse =
        Parsing.tryParse ContentLocation.Mapping.Parse

    override x.ToString () =
        ContentLocation.Format x

and ContentLocationUri =
    | Absolute of AbsoluteUri
    | Partial of PartialUri

(* Method

   Taken from RFC 7231, Section 4
   See [http://tools.ietf.org/html/rfc7231#section-4] *)

type Method =
    | CONNECT
    | DELETE 
    | HEAD 
    | GET 
    | OPTIONS 
    | POST 
    | PUT 
    | TRACE 
    | Custom of string

    static member Mapping =

        let methodP =
            choice [
                skipStringCI "connect" >>% CONNECT
                skipStringCI "delete" >>% DELETE
                skipStringCI "head" >>% HEAD
                skipStringCI "get" >>% GET
                skipStringCI "options" >>% OPTIONS
                skipStringCI "post" >>% POST
                skipStringCI "put" >>% PUT
                skipStringCI "trace" >>% TRACE
                restOfLine false |>> Method.Custom ]

        let methodF =
            function | CONNECT -> append "CONNECT"
                     | DELETE -> append "DELETE" 
                     | HEAD -> append "HEAD" 
                     | GET -> append "GET" 
                     | OPTIONS -> append "OPTIONS"
                     | POST -> append "POST" 
                     | PUT -> append "PUT"  
                     | TRACE -> append "TRACE"
                     | Method.Custom x -> append x

        { Parse = methodP
          Format = methodF }

    static member Format =
        Formatting.format Method.Mapping.Format

    static member Parse =
        Parsing.parse Method.Mapping.Parse

    override x.ToString () =
        Method.Format x

(* Expect

   Taken from RFC 7231, Section 5.1.1 Expect
   See [http://tools.ietf.org/html/rfc7231#section-5.1.1] *)

type Expect =
    | Expect of Continue

    static member Mapping =

        let expectP =
            skipStringCI "100-continue" >>% Expect Continue

        let expectF =
            function | Expect Continue -> append "100-continue"

        { Parse = expectP
          Format = expectF }

    static member Format =
        Formatting.format Expect.Mapping.Format

    static member Parse =
        Parsing.parse Expect.Mapping.Parse

    static member TryParse =
        Parsing.tryParse Expect.Mapping.Parse

    override x.ToString () =
        Expect.Format x

and Continue =
    | Continue

(* Max-Forwards

   Taken from RFC 7231, Section 5.1.2. Max-Forwards
   [http://tools.ietf.org/html/rfc7231#section-5.1.2] *)

type MaxForwards =
    | MaxForwards of int

    static member Mapping =

        let maxForwardsP =
            puint32 |>> (int >> MaxForwards)

        let maxForwardsF =
            function | MaxForwards x -> append (string x)

        { Parse = maxForwardsP
          Format = maxForwardsF }

    static member Format =
        Formatting.format MaxForwards.Mapping.Format

    static member Parse =
        Parsing.parse MaxForwards.Mapping.Parse

    static member TryParse =
        Parsing.tryParse MaxForwards.Mapping.Parse

    override x.ToString () =
        MaxForwards.Format x

(* Quality Values

   Taken from RFC 7231, Section 5.3.1. Quality Values
   [http://tools.ietf.org/html/rfc7231#section-5.3.1] *)

type Weight =
    | Weight of float

    static member Mapping =

        let valueOrDefault =
            function | Some x -> float (sprintf "0.%s" x)
                     | _ -> 0.

        let d3P =
                manyMinMaxSatisfy 0 3 (int >> Grammar.digit) 
            .>> notFollowedBy (skipSatisfy (int >> Grammar.digit))

        let d03P =
                skipManyMinMaxSatisfy 0 3 ((=) '0') 
            .>> notFollowedBy (skipSatisfy (int >> Grammar.digit))

        let qvalueP =
            choice [ 
                skipChar '0' >>. opt (skipChar '.' >>. d3P) |>> valueOrDefault
                skipChar '1' >>. optional (skipChar '.' >>. d03P) >>% 1. ]

        let weightP =
            semicolonP >>. owsP >>. skipStringCI "q=" >>. qvalueP .>> owsP |>> Weight

        let weightF =
            function | Weight x -> appendf1 ";q={0:G4}" x

        { Parse = weightP
          Format = weightF }

(* Accept

   Taken from RFC 7231, Section 5.3.2. Accept
   [http://tools.ietf.org/html/rfc7231#section-5.3.2] *)

type Accept =
    | Accept of AcceptableMedia list

    static member Mapping =

        let acceptP =
            infixP AcceptableMedia.Mapping.Parse commaP |>> Accept

        let acceptF =
            function | Accept x -> join AcceptableMedia.Mapping.Format commaF x

        { Parse = acceptP
          Format = acceptF }

    static member Format =
        Formatting.format Accept.Mapping.Format

    static member Parse =
        Parsing.parse Accept.Mapping.Parse

    static member TryParse =
        Parsing.tryParse Accept.Mapping.Parse

    override x.ToString () =
        Accept.Format x

and AcceptableMedia =
    | AcceptableMedia of MediaRange * AcceptParameters option

    static member Mapping =

        let acceptableMediaP = 
            MediaRange.Mapping.Parse .>>. opt AcceptParameters.Mapping.Parse
            |>> AcceptableMedia

        let acceptableMediaF =
            function | AcceptableMedia (m, p) -> 
                         MediaRange.Mapping.Format m 
                         >> (function | Some p -> AcceptParameters.Mapping.Format p
                                      | _ -> id) p

        { Parse = acceptableMediaP
          Format = acceptableMediaF }

and MediaRange =
    | Closed of Type * SubType * Parameters
    | Partial of Type * Parameters
    | Open of Parameters

    static member Mapping =

        let mediaRangeParameterP =
            notFollowedBy (owsP >>. skipStringCI "q=") >>. tokenP .>> skipChar '=' .>>. tokenP

        let mediaRangeParametersP =
            prefixP mediaRangeParameterP semicolonP |>> Map.ofList

        let openMediaRangeP = 
            skipString "*/*" >>. owsP >>. mediaRangeParametersP |>> (Parameters >> MediaRange.Open)

        let partialMediaRangeP = 
            tokenP .>> skipString "/*" .>> owsP .>>. mediaRangeParametersP
            |>> fun (x, parameters) -> 
                    MediaRange.Partial (Type x, Parameters parameters)

        let closedMediaRangeP = 
            tokenP .>> skipChar '/' .>>. tokenP .>> owsP .>>. mediaRangeParametersP
            |>> fun ((x, y), parameters) -> 
                    MediaRange.Closed (Type x, SubType y, Parameters parameters)

        let mediaRangeP = 
            choice [
                attempt openMediaRangeP
                attempt partialMediaRangeP
                closedMediaRangeP ]

        let mediaRangeF =
            function | MediaRange.Closed (Type x, SubType y, p) -> appendf2 "{0}/{1}" x y >> Parameters.Mapping.Format p
                     | MediaRange.Partial (Type x, p) -> appendf1 "{0}/*" x >> Parameters.Mapping.Format p
                     | MediaRange.Open p -> append "*/*" >> Parameters.Mapping.Format p

        { Parse = mediaRangeP
          Format = mediaRangeF }

and AcceptParameters =
    | AcceptParameters of Weight * AcceptExtensions

    static member Mapping =

        let acceptParamsP =
            Weight.Mapping.Parse .>> owsP .>>. AcceptExtensions.Mapping.Parse
            |>> AcceptParameters

        let acceptParamsF =
            function | AcceptParameters (w, e) -> 
                        Weight.Mapping.Format w >> AcceptExtensions.Mapping.Format e

        { Parse = acceptParamsP
          Format = acceptParamsF }

and AcceptExtensions =
    | Extensions of Map<string, string option>

    static member Mapping =

        let acceptExtP =
            tokenP .>>. opt (skipChar '=' >>. (quotedStringP <|> tokenP))

        let acceptExtsP =
            prefixP acceptExtP semicolonP |>> (Map.ofList >> Extensions)

        let acceptExtsF =
            function | Extensions (x: Map<string, string option>) when Map.isEmpty x -> id
                     | _ -> id

        { Parse = acceptExtsP
          Format = acceptExtsF }

(* Accept-Charset

   Taken from RFC 7231, Section 5.3.3 Accept-Charset
   [http://tools.ietf.org/html/rfc7231#section-5.3.3] *)

type AcceptCharset =
    | AcceptCharset of AcceptableCharset list

    static member Mapping =

        let acceptCharsetP =
            infix1P AcceptableCharset.Mapping.Parse commaP
            |>> AcceptCharset

        let acceptCharsetF =
            function | AcceptCharset x ->
                        join AcceptableCharset.Mapping.Format commaF x

        { Parse = acceptCharsetP
          Format = acceptCharsetF }

    static member Format =
        Formatting.format AcceptCharset.Mapping.Format

    static member Parse =
        Parsing.parse AcceptCharset.Mapping.Parse

    static member TryParse =
        Parsing.tryParse AcceptCharset.Mapping.Parse

    override x.ToString () =
        AcceptCharset.Format x

and AcceptableCharset =
    | AcceptableCharset of CharsetRange * Weight option

    static member Mapping =

        let acceptableCharsetP =
            CharsetRange.Mapping.Parse .>> owsP .>>. opt Weight.Mapping.Parse
            |>> AcceptableCharset

        let acceptableCharsetF =
            function | AcceptableCharset (c, w) ->
                        CharsetRange.Mapping.Format c 
                        >> (function | Some w -> Weight.Mapping.Format w
                                     | _ -> id) w

        { Parse = acceptableCharsetP
          Format = acceptableCharsetF }

and CharsetRange =
    | Charset of Charset
    | Any

    static member Mapping =

        let charsetRangeAnyP =
            skipChar '*' >>% CharsetRange.Any

        let charsetRangeCharsetP =
            tokenP |>> fun s -> Charset (Charset.Charset s)

        let charsetRangeP = 
            choice [
                attempt charsetRangeAnyP
                charsetRangeCharsetP ]

        let charsetRangeF =
            function | Charset (Charset.Charset x) -> append x
                     | Any -> append "*"

        { Parse = charsetRangeP
          Format = charsetRangeF }

and Charset =
    | Charset of string

(* Charset Presets *)

type Charset with

    /// Convenience definition for "iso-8859-1"
    static member Iso88591 =
        Charset "iso-8859-1"

    /// Convenience definition for "unicode-1-1"
    static member Unicode =
        Charset "unicode-1-1"

    /// Convenience definition for "utf-8"
    static member Utf8 =
        Charset "utf-8"

(* Accept-Encoding

   Taken from RFC 7231, Section 5.3.4. Accept-Encoding
   [http://tools.ietf.org/html/rfc7231#section-5.3.4] *)

type AcceptEncoding =
    | AcceptEncoding of AcceptableEncoding list

    static member Mapping =

        let acceptEncodingP =
            infixP AcceptableEncoding.Mapping.Parse commaP
            |>> AcceptEncoding

        let acceptEncodingF =
            function | AcceptEncoding x ->
                        join AcceptableEncoding.Mapping.Format commaF x

        { Parse = acceptEncodingP
          Format = acceptEncodingF }

    static member Format =
        Formatting.format AcceptEncoding.Mapping.Format

    static member Parse =
        Parsing.parse AcceptEncoding.Mapping.Parse

    static member TryParse =
        Parsing.tryParse AcceptEncoding.Mapping.Parse

    override x.ToString () =
        AcceptEncoding.Format x

and AcceptableEncoding =
    | AcceptableEncoding of EncodingRange * Weight option

    static member Mapping =

        let acceptableEncodingP =
            EncodingRange.Mapping.Parse .>> owsP .>>. opt Weight.Mapping.Parse
            |>> AcceptableEncoding

        let acceptableEncodingF =
            function | AcceptableEncoding (e, w) ->
                        EncodingRange.Mapping.Format e
                        >> (function | Some w -> Weight.Mapping.Format w
                                     | _ -> id) w

        { Parse = acceptableEncodingP
          Format = acceptableEncodingF }

and EncodingRange =
    | Coding of ContentCoding
    | Identity
    | Any

    static member Mapping =

        let encodingRangeAnyP =
            skipChar '*' >>% Any

        let encodingRangeIdentityP =
            skipStringCI "identity" >>% Identity

        let encodingRangeCodingP =
            tokenP |>> fun s -> Coding (ContentCoding s)

        let encodingRangeP =
            choice [
                attempt encodingRangeAnyP
                attempt encodingRangeIdentityP
                encodingRangeCodingP ]

        let encodingRangeF =
            function | Coding (ContentCoding x) -> append x
                     | Identity -> append "identity"
                     | Any -> append "*"

        { Parse = encodingRangeP
          Format = encodingRangeF }

(* Accept-Language

   Taken from RFC 7231, Section 5.3.5. Accept-Language
   [http://tools.ietf.org/html/rfc7231#section-5.3.5] *)

type AcceptLanguage =
    | AcceptLanguage of AcceptableLanguage list

    static member Mapping =

        let acceptLanguageP =
            infixP AcceptableLanguage.Mapping.Parse commaP
            |>> AcceptLanguage

        let acceptLanguageF =
            function | AcceptLanguage x ->
                        join AcceptableLanguage.Mapping.Format commaF x

        { Parse = acceptLanguageP
          Format = acceptLanguageF }

    static member Format =
        Formatting.format AcceptLanguage.Mapping.Format

    static member Parse =
        Parsing.parse AcceptLanguage.Mapping.Parse

    static member TryParse =
        Parsing.tryParse AcceptLanguage.Mapping.Parse

    override x.ToString () =
        AcceptLanguage.Format x

and AcceptableLanguage =
    | AcceptableLanguage of LanguageRange * Weight option

    static member Mapping =

        let acceptableLanguageP =
            LanguageRange.Mapping.Parse .>> owsP .>>. opt Weight.Mapping.Parse
            |>> AcceptableLanguage

        let acceptableLanguageF =
            function | AcceptableLanguage (l, w) ->
                        LanguageRange.Mapping.Format l
                        >> (function | Some w -> Weight.Mapping.Format w
                                     | _ -> id) w

        { Parse = acceptableLanguageP
          Format = acceptableLanguageF }

(* Referer

   Taken from RFC 7231, Section 5.5.2 Referer
   [http://tools.ietf.org/html/rfc7231#section-5.5.2] *)

type Referer =
    | Referer of RefererUri

    static member Mapping =

        let refererP =
            choice [
                attempt AbsoluteUri.Mapping.Parse |>> (Absolute >> Referer)
                PartialUri.Mapping.Parse |>> (Partial >> Referer) ]

        let refererF =
            function | Referer (Absolute x) -> AbsoluteUri.Mapping.Format x
                     | Referer (Partial x) -> PartialUri.Mapping.Format x

        { Parse = refererP
          Format = refererF }

    static member Format =
        Formatting.format Referer.Mapping.Format

    static member Parse =
        Parsing.parse Referer.Mapping.Parse

    static member TryParse =
        Parsing.tryParse Referer.Mapping.Parse

    override x.ToString () =
        Referer.Format x
    
and RefererUri =
    | Absolute of AbsoluteUri
    | Partial of PartialUri

(* HTTP-Date

   Taken from RFC 7231, Section 7.1.1.1 HTTP-Date *)

let private dateTimeFormat =
    CultureInfo.InvariantCulture.DateTimeFormat

let private dateTimeAdjustment =
    DateTimeStyles.AdjustToUniversal

let internal httpDateP : Parser<DateTime, unit> =
    restOfLine false >>= (fun s ->
        match DateTime.TryParse (s, dateTimeFormat, dateTimeAdjustment) with
        | true, d -> preturn d
        | _ -> pzero)

(* Date

   Taken from RFC 7231, Section 7.1.1.2 Date
   [http://tools.ietf.org/html/rfc7231#section-7.1.1.2] *)

type Date =
    | Date of DateTime

    static member Mapping =

        let dateP =
            httpDateP |>> Date.Date

        let dateF =
            function | Date.Date x -> append (x.ToString "r")

        { Parse = dateP
          Format = dateF }

    static member Format =
        Formatting.format Date.Mapping.Format

    static member Parse =
        Parsing.parse Date.Mapping.Parse

    static member TryParse =
        Parsing.tryParse Date.Mapping.Parse

    override x.ToString () =
        Date.Format x

(* Location

   Taken from RFC 7231, Section 7.1.2 Location
   [http://tools.ietf.org/html/rfc7231#section-7.1.2] *)

type Location =
    | Location of UriReference

    static member Mapping =

        let locationP =
            UriReference.Mapping.Parse |>> Location

        let locationF =
            function | Location x -> UriReference.Mapping.Format x

        { Parse = locationP
          Format = locationF }

    static member Format =
        Formatting.format Location.Mapping.Format

    static member Parse =
        Parsing.parse Location.Mapping.Parse

    static member TryParse =
        Parsing.tryParse Location.Mapping.Parse

    override x.ToString () =
        Location.Format x

(* Retry-After

   Taken from RFC 7231, Section 7.1.3. Retry-After
   [http://tools.ietf.org/html/rfc7231#section-7.1.3] *)

type RetryAfter =
    | RetryAfter of RetryAfterChoice

    static member Mapping =

        let retryAfterP =
            choice [
                attempt httpDateP |>> (Date >> RetryAfter)
                puint32 |>> (float >> TimeSpan.FromSeconds >> Delay >> RetryAfter) ]

        let retryAfterF =
            function | RetryAfter (Date x) -> append (x.ToString "r")
                     | RetryAfter (Delay x) -> append (string (int x.TotalSeconds))

        { Parse = retryAfterP
          Format = retryAfterF }

    static member Format =
        Formatting.format RetryAfter.Mapping.Format

    static member Parse =
        Parsing.parse RetryAfter.Mapping.Parse

    static member TryParse =
        Parsing.tryParse RetryAfter.Mapping.Parse

    override x.ToString () =
        RetryAfter.Format x

and RetryAfterChoice =
    | Date of DateTime
    | Delay of TimeSpan

(* Allow

   Taken from RFC 7231, Section 7.4.1 Allow
   [http://tools.ietf.org/html/rfc7231#section-7.4.1] *)

type Allow =
    | Allow of Method list

    static member Mapping =

        let allowP =
            infixP Method.Mapping.Parse commaP |>> Allow

        let allowF =
            function | Allow x -> join Method.Mapping.Format commaF x

        { Parse = allowP
          Format = allowF }

    static member Format =
        Formatting.format Allow.Mapping.Format

    static member Parse =
        Parsing.parse Allow.Mapping.Parse

    static member TryParse =
        Parsing.tryParse Allow.Mapping.Parse

    override x.ToString () =
        Allow.Format x

(* RFC 7232

   Types, parsers and formatters implemented to mirror the semantics of
   HTTP as defined in RFC 7232.

   See [http://tools.ietf.org/html/rfc7232] *)

(* Last-Modified

   Taken from RFC 7232, Section 2.2 Last-Modified
   [http://tools.ietf.org/html/rfc7232#section-2.2] *)

type LastModified =
    | LastModified of DateTime

    static member Mapping =

        let lastModifiedP =
            httpDateP |>> LastModified

        let lastModifiedF =
            function | LastModified x -> append (x.ToString "r")

        { Parse = lastModifiedP
          Format = lastModifiedF }

    static member Format =
        Formatting.format LastModified.Mapping.Format

    static member Parse =
        Parsing.parse LastModified.Mapping.Parse

    static member TryParse =
        Parsing.tryParse LastModified.Mapping.Parse

    override x.ToString () =
        LastModified.Format x

(* ETag

   Taken from RFC 7232 Section 2.3 ETag
   [http://tools.ietf.org/html/rfc7232#section-2.3] *)

type ETag =
    | ETag of EntityTag

    static member Mapping =

        let eTagP =
            EntityTag.Mapping.Parse |>> ETag

        let eTagF =
            function | ETag x -> EntityTag.Mapping.Format x

        { Parse = eTagP
          Format = eTagF }

    static member Format =
        Formatting.format ETag.Mapping.Format

    static member Parse =
        Parsing.parse ETag.Mapping.Parse

    static member TryParse =
        Parsing.tryParse ETag.Mapping.Parse

    override x.ToString () =
        ETag.Format x

and EntityTag =
    | Strong of string
    | Weak of string

    static member Mapping =

        let eTagChar i =
                (i = 0x21)
             || (i >= 0x23 && i <= 0x7e)
             || (obstext i)

        let opaqueTagP =
                skipSatisfy (int >> Grammar.dquote) 
            >>. manySatisfy (int >> eTagChar) 
            .>> skipSatisfy (int >> Grammar.dquote)

        let entityTagP =
            choice [
                attempt (skipString "W/" >>. opaqueTagP |>> Weak)
                opaqueTagP |>> Strong ]

        let entityTagF =
            function | Strong x -> appendf1 "\"{0}\"" x
                     | Weak x -> appendf1 "W/\"{0}\"" x

        { Parse = entityTagP
          Format = entityTagF }

(* If-Match

   Taken from RFC 7232, Section 3.1, If-Match
   [http://tools.ietf.org/html/rfc7232#section-3.1] *)

type IfMatch =
    | IfMatch of IfMatchChoice

    static member Mapping =

        let ifMatchP =
            choice [
                skipChar '*' >>% IfMatch (Any)
                infixP EntityTag.Mapping.Parse commaP |>> (EntityTags >> IfMatch) ]

        let ifMatchF =
            function | IfMatch (EntityTags x) -> join EntityTag.Mapping.Format commaF x
                     | IfMatch (Any) -> append "*"

        { Parse = ifMatchP
          Format = ifMatchF }

    static member Format =
        Formatting.format IfMatch.Mapping.Format

    static member Parse =
        Parsing.parse IfMatch.Mapping.Parse

    static member TryParse =
        Parsing.tryParse IfMatch.Mapping.Parse

    override x.ToString () =
        IfMatch.Format x

and IfMatchChoice =
    | EntityTags of EntityTag list
    | Any

(* If-None-Match

   Taken from RFC 7232, Section 3.2, If-None-Match
   [http://tools.ietf.org/html/rfc7232#section-3.2] *)

type IfNoneMatch =
    | IfNoneMatch of IfNoneMatchChoice

    static member Mapping =

        let ifNoneMatchP =
            choice [
                skipChar '*' >>% IfNoneMatch (Any)
                infixP EntityTag.Mapping.Parse commaP |>> (EntityTags >> IfNoneMatch) ]

        let ifNoneMatchF =
            function | IfNoneMatch (EntityTags x) -> join EntityTag.Mapping.Format commaF x
                     | IfNoneMatch (Any) -> append "*"

        { Parse = ifNoneMatchP
          Format = ifNoneMatchF }

    static member Format =
        Formatting.format IfNoneMatch.Mapping.Format

    static member Parse =
        Parsing.parse IfNoneMatch.Mapping.Parse

    static member TryParse =
        Parsing.tryParse IfNoneMatch.Mapping.Parse

    override x.ToString () =
        IfNoneMatch.Format x

and IfNoneMatchChoice =
    | EntityTags of EntityTag list
    | Any

(* If-Modified-Since

   Taken from RFC 7232, Section 3.3, If-Modified-Since
   [http://tools.ietf.org/html/rfc7232#section-3.3] *)

type IfModifiedSince =
    | IfModifiedSince of DateTime

    static member Mapping =

        let ifModifiedSinceP =
            httpDateP |>> IfModifiedSince

        let ifModifiedSinceF =
            function | IfModifiedSince x -> append (x.ToString "r")

        { Parse = ifModifiedSinceP
          Format = ifModifiedSinceF }

    static member Format =
        Formatting.format IfModifiedSince.Mapping.Format

    static member Parse =
        Parsing.parse IfModifiedSince.Mapping.Parse

    static member TryParse =
        Parsing.tryParse IfModifiedSince.Mapping.Parse

    override x.ToString () =
        IfModifiedSince.Format x

(* If-Unmodified-Since

   Taken from RFC 7232, Section 3.4, If-Unmodified-Since
   [http://tools.ietf.org/html/rfc7232#section-3.4] *)

type IfUnmodifiedSince =
    | IfUnmodifiedSince of DateTime

    static member Mapping =

        let ifUnmodifiedSinceP =
            httpDateP |>> IfUnmodifiedSince

        let ifUnmodifiedSinceF =
            function | IfUnmodifiedSince x -> append (x.ToString "r")

        { Parse = ifUnmodifiedSinceP
          Format = ifUnmodifiedSinceF }

    static member Format =
        Formatting.format IfUnmodifiedSince.Mapping.Format

    static member Parse =
        Parsing.parse IfUnmodifiedSince.Mapping.Parse

    static member TryParse =
        Parsing.tryParse IfUnmodifiedSince.Mapping.Parse

    override x.ToString () =
        IfUnmodifiedSince.Format x

(* RFC 7233

   Types, parsers and formatters implemented to mirror the semantics of
   HTTP as defined in RFC 7233.
   
   See [http://tools.ietf.org/html/rfc7233] *)

(* If-Range

   Taken from RFC 7233, Section 3.2 If-Range
   See [http://tools.ietf.org/html/rfc7233#section-3.2] *)

type IfRange =
    | IfRange of IfRangeChoice

    static member Mapping =

        let ifRangeP =
            (EntityTag.Mapping.Parse |>> (EntityTag >> IfRange)) <|> 
                                             (httpDateP |>> (Date >> IfRange))

        let ifRangeF =
            function | IfRange (Date x) -> append (x.ToString "r")
                     | IfRange (EntityTag x) -> EntityTag.Mapping.Format x
        
        { Parse = ifRangeP
          Format = ifRangeF }

    static member Format =
        Formatting.format IfRange.Mapping.Format

    static member Parse =
        Parsing.parse IfRange.Mapping.Parse

    static member TryParse =
        Parsing.tryParse IfRange.Mapping.Parse

    override x.ToString () =
        IfRange.Format x

and IfRangeChoice =
    | Date of DateTime
    | EntityTag of EntityTag

(* RFC 7234

   Types, parsers and formatters implemented to mirror the semantics of
   HTTP as defined in RFC 7234.

   See [http://tools.ietf.org/html/rfc7234] *)

(* Age

   Taken from RFC 7234 Section 5.1 Age
   See [http://tools.ietf.org/html/rfc7234#section-5.1] *)

type Age =
    | Age of TimeSpan

    static member Mapping =

        let ageP =
            puint32 |>> (float >> TimeSpan.FromSeconds >> Age)

        let ageF =
            function | Age x -> append (string x.TotalSeconds)

        { Parse = ageP
          Format = ageF }

    static member Format =
        Formatting.format Age.Mapping.Format

    static member Parse =
        Parsing.parse Age.Mapping.Parse

    static member TryParse =
        Parsing.tryParse Age.Mapping.Parse

    override x.ToString () =
        Age.Format x

(* Cache-Control

   Taken from RFC 7234 Section 5.2 Cache-Control
   See [http://tools.ietf.org/html/rfc7234#section-5.2]

   Note that from a type system perspective we don't currently
   distinguish between cache-directives that are valid for
   requests/responses or both. This may be worth changing in future,
   but for now it should hopefully be clear enough when used. *)

type CacheControl =
    | CacheControl of CacheDirective list

    static member Mapping =

        let cacheControlP =
            infix1P CacheDirective.Mapping.Parse commaP |>> CacheControl

        let cacheControlF =
            function | CacheControl x -> join CacheDirective.Mapping.Format commaF x

        { Parse = cacheControlP
          Format = cacheControlF }

    static member Format =
        Formatting.format CacheControl.Mapping.Format

    static member Parse =
        Parsing.parse CacheControl.Mapping.Parse

    static member TryParse =
        Parsing.tryParse CacheControl.Mapping.Parse

    override x.ToString () =
        CacheControl.Format x

and CacheDirective =
    | MaxAge of TimeSpan
    | MaxStale of TimeSpan
    | MinFresh of TimeSpan
    | MustRevalidate
    | NoCache
    | NoStore
    | NoTransform
    | OnlyIfCached
    | Private
    | ProxyRevalidate
    | Public
    | SMaxAge of TimeSpan
    | Custom of string * string option

    static member Mapping =

        // TODO: Custom Directive Parsing

        let cacheDirectiveP =
            choice [
                attempt (skipStringCI "max-age=" >>. puint32 |>> (float >> TimeSpan.FromSeconds >> MaxAge))
                attempt (skipStringCI "max-stake=" >>. puint32 |>> (float >> TimeSpan.FromSeconds >> MaxStale))
                attempt (skipStringCI "min-fresh=" >>. puint32 |>> (float >> TimeSpan.FromSeconds >> MinFresh))
                attempt (skipStringCI "must-revalidate" >>% MustRevalidate)
                attempt (skipStringCI "no-cache" >>% NoCache)
                attempt (skipStringCI "no-store" >>% NoStore)
                attempt (skipStringCI "no-transform" >>% NoTransform)
                attempt (skipStringCI "only-if-cached" >>% OnlyIfCached)
                attempt (skipStringCI "private" >>% Private)
                attempt (skipStringCI "proxy-revalidate" >>% ProxyRevalidate)
                attempt (skipStringCI "public" >>% Public)
                attempt (skipStringCI "s-maxage=" >>. puint32 |>> (float >> TimeSpan.FromSeconds >> SMaxAge)) ] 

        let cacheDirectiveF =
            function | MaxAge x -> appendf1 "max-age={0}" (int x.TotalSeconds)
                     | MaxStale x -> appendf1 "max-stale={0}" (int x.TotalSeconds)
                     | MinFresh x -> appendf1 "min-fresh={0}" (int x.TotalSeconds)
                     | MustRevalidate -> append "must-revalidate"
                     | NoCache -> append "no-cache"
                     | NoStore -> append "no-store"
                     | NoTransform -> append "no-transform"
                     | OnlyIfCached -> append "only-if-cached"
                     | Private -> append "private"
                     | ProxyRevalidate -> append "proxy-revalidate"
                     | Public -> append "public"
                     | SMaxAge x -> appendf1 "s-maxage={0}" x
                     | Custom (x, Some y) -> appendf2 "{0}={2}" x y
                     | Custom (x, _) -> append x

        { Parse = cacheDirectiveP
          Format = cacheDirectiveF }

(* Expires

   Taken from RFC 7234 Section 5.3 Expires
   See [http://tools.ietf.org/html/rfc7234#section-5.3] *)

type Expires =
    | Expires of DateTime

    static member Mapping =

        let expiresP =
            httpDateP |>> Expires

        let expiresF =
            function | Expires x -> append (x.ToString "r")

        { Parse = expiresP
          Format = expiresF }

    static member Format =
        Formatting.format Expires.Mapping.Format

    static member Parse =
        Parsing.parse Expires.Mapping.Parse

    static member TryParse =
        Parsing.tryParse Expires.Mapping.Parse

    override x.ToString () =
        Expires.Format x