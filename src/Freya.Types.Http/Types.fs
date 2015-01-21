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
//----------------------------------------------------------------------------

[<AutoOpen>]
module Freya.Types.Http.Types

#nowarn "60"

open System
open System.Globalization
open System.Runtime.CompilerServices
open Aether
open FParsec
open Freya.Types
open Freya.Types.Language
open Freya.Types.Uri

(* Internals *)

[<assembly:InternalsVisibleTo ("Freya.Types.Cors")>]
do ()

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

let internal partialUriF =
    function | { PartialUri.Relative = r
                 Query = q } ->
                    let formatters =
                        [ relativePartF r
                          (function | Some q -> queryF q | _ -> id) q ]

                    fun b -> List.fold (fun b f -> f b) b formatters

let internal partialUriP =
    relativePartP .>>. opt queryP
    |>> fun (relative, query) ->
        { Relative = relative
          Query = query }

type PartialUri with

    static member Format =
        format partialUriF

    static member Parse =
        parseExact partialUriP

    static member TryParse =
        parseOption partialUriP

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

let private httpVersionF =
    function | HttpVersion.HTTP x -> appendf1 "HTTP/{0:G4}" x 
             | HttpVersion.Custom x -> append x

let private httpVersionP =
    choice [
        skipString "HTTP/1.0" >>% HttpVersion.HTTP 1.0
        skipString "HTTP/1.1" >>% HttpVersion.HTTP 1.1
        restOfLine false |>> HttpVersion.Custom ]

let formatHttpVersion =
    format httpVersionF

type HttpVersion with

    static member Format =
        format httpVersionF

    static member Parse =
        parseExact httpVersionP

    override x.ToString () =
        HttpVersion.Format x

(* Content-Length

   Taken from RFC 7230, Section 3.3.2 Content-Length
   See [http://tools.ietf.org/html/rfc7230#section-3.3.2] *)

type ContentLength =
    | ContentLength of int

let private contentLengthF =
    function | ContentLength x -> append (string x)

let private contentLengthP =
    puint32 |>> (int >> ContentLength)

type ContentLength with

    static member Format =
        format contentLengthF

    static member Parse =
        parseExact contentLengthP

    static member TryParse =
        parseOption contentLengthP

    override x.ToString () =
        ContentLength.Format x

(* Host

   Taken from RFC 7230, Section 5.4 Host
   See [http://tools.ietf.org/html/rfc7230#section-5.4] *)

type Host =
    | Host of Types.Host * Port option

let private hostF =
    function | Host (h, p) ->
                let formatters =
                    [ hostF h
                      (function | Some p -> portF p | _ -> id) p ]

                fun b -> List.fold (fun b f -> f b) b formatters

let private hostP =
    hostP .>>. opt portP |>> Host

type Host with

    static member Format =
        format hostF

    static member Parse =
        parseExact hostP

    static member TryParse =
        parseOption hostP

    override x.ToString () =
        Host.Format x

(* Connection

   Taken from RFC 7230, Section 6.1 Connection
   See [http://tools.ietf.org/html/rfc7230#section-6.1] *)

type Connection =
    | Connection of ConnectionOption list

and ConnectionOption =
    | ConnectionOption of string

let private connectionOptionF =
    function | ConnectionOption x -> append x

let private connectionF =
    function | Connection x -> join connectionOptionF commaF x

let private connectionP =
    infix1P tokenP commaP |>> (List.map ConnectionOption >> Connection)

type Connection with

    static member Format =
        format connectionF

    static member Parse =
        parseExact connectionP

    static member TryParse =
        parseOption connectionP

    override x.ToString () =
        Connection.Format x

(* RFC 7231

   Types, parsers and formatters implemented to mirror the semantics of
   HTTP as defined in RFC 7231.
   
   See [http://tools.ietf.org/html/rfc7231] *)

(* Media-Type

   Includes the common definition of parameter as defined within this
   section, but applicable to multiple later types.

   Taken from RFC 7231, Section 3.1.1.1 Media-Type
   [http://tools.ietf.org/html/rfc7231#section-3.1.1.1] *)

(* Parameters *)

type Parameters =
    Map<string, string>

let private pairF =
    (<||) (appendf2 "{0}={1}")

let internal parametersF =
    function | (x: Map<string, string>) when Map.isEmpty x -> id
             | (x) -> append ";" >> join pairF semicolonF (Map.toList x |> List.rev)

let private parameterP =
    tokenP .>> skipChar '=' .>>. (quotedStringP <|> tokenP)

let private parametersP =
    prefixP parameterP semicolonP |>> Map.ofList

(* Media-Type *)

type MediaType =
    | MediaType of Type * SubType * Parameters

    static member TypeLens : Lens<MediaType, Type> =
        (fun (MediaType (x, _, _)) -> x), (fun x (MediaType (_, y, z)) -> MediaType (x, y, z))

    static member SubTypeLens : Lens<MediaType, SubType> =
        (fun (MediaType (_, y, _)) -> y), (fun y (MediaType (x, _, z)) -> MediaType (x, y, z))

    static member ParametersLens : Lens<MediaType, Parameters> =
        (fun (MediaType (_, _, z)) -> z), (fun z (MediaType (x, y, _)) -> MediaType (x, y, z))

and Type =
    | Type of string

and SubType =
    | SubType of string

(* Presets *)

type MediaType with

    static member HTML =
        MediaType (Type "text", SubType "html", Map.empty)

    static member JSON =
        MediaType (Type "application", SubType "json", Map.empty)

    /// Convenience definition for "text/plain" without extra parameters
    static member Text =
        MediaType (Type "text", SubType "plain", Map.empty)

    /// Convenience definition for "application/xml" without extra parameters
    static member XML =
        MediaType (Type "application", SubType "xml", Map.empty)

(* Formatting *)

let private mediaTypeF =
    function | MediaType (Type x, SubType y, p) -> appendf2 "{0}/{1}" x y >> parametersF p

(* Parsing *)

let private mediaTypeP =
    tokenP .>> slashP .>>. tokenP .>>. parametersP
    |>> (fun ((x, y), p) -> MediaType (Type x, SubType y, p))

(* Augmentation *)

type MediaType with

    static member Format =
        format mediaTypeF

    static member Parse =
        parseExact mediaTypeP

    static member TryParse =
        parseOption mediaTypeP

    override x.ToString () =
        MediaType.Format x

(* Content-Type

    Taken from RFC 7231, Section 3.1.1.5 Content-Type
    [http://tools.ietf.org/html/rfc7231#section-3.1.1.5] *)

type ContentType =
    | ContentType of MediaType

    static member MediaTypeLens : Lens<ContentType, MediaType> =
        (fun (ContentType x) -> x), (fun x _ -> ContentType x)

let private contentTypeF =
    function | ContentType x -> mediaTypeF x

let private contentTypeP =
    mediaTypeP |>> ContentType

type ContentType with

    static member Format =
        format contentTypeF

    static member Parse =
        parseExact contentTypeP

    static member TryParse =
        parseOption contentTypeP

    override x.ToString () =
        ContentType.Format x

(* Content-Encoding

   Taken from RFC 7231, Section 3.1.2.2 Content-Encoding
   [http://tools.ietf.org/html/rfc7231#section-3.1.2.2] *)

type ContentEncoding =
    | ContentEncoding of ContentCoding list

and ContentCoding =
    | ContentCoding of string

(* Presets *)

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

(* Formatting *)

let private contentCodingF =
    function | ContentCoding x -> append x

let private contentEncodingF =
    function | ContentEncoding x -> join contentCodingF commaF x

(* Parsing *)

let private contentEncodingP =
    infix1P tokenP commaP |>> (List.map ContentCoding >> ContentEncoding)

(* Augmentation *)

type ContentEncoding with

    static member Format =
        format contentEncodingF

    static member Parse =
        parseExact contentEncodingP

    static member TryParse =
        parseOption contentEncodingP

    override x.ToString () =
        ContentEncoding.Format x

(* Content-Language

   Taken from RFC 7231, Section 3.1.3.2 Content-Language
   [http://tools.ietf.org/html/rfc7231#section-3.1.3.2] *)

type ContentLanguage =
    | ContentLanguage of LanguageTag list

let private contentLanguageF =
    function | ContentLanguage xs -> join languageTagF commaF xs

let private contentLanguageP =
    infix1P languageTagP commaP |>> ContentLanguage

type ContentLanguage with

    static member Format =
        format contentLanguageF

    static member Parse =
        parseExact contentLanguageP

    static member TryParse =
        parseOption contentLanguageP

    override x.ToString () =
        ContentLanguage.Format x

(* Content-Location

   Taken from RFC 7231, Section 3.1.4.2 Content-Location
   [http://tools.ietf.org/html/rfc7231#section-3.1.4.2] *)

type ContentLocation =
    | ContentLocation of ContentLocationUri

and ContentLocationUri =
    | Absolute of AbsoluteUri
    | Partial of PartialUri

let private contentLocationF =
    function | ContentLocation (Absolute x) -> absoluteUriF x
             | ContentLocation (Partial x) -> partialUriF x

let private contentLocationP =
    choice [
        attempt absoluteUriP |>> (Absolute >> ContentLocation)
        partialUriP |>> (Partial >> ContentLocation) ]

type ContentLocation with

    static member Format =
        format contentLocationF

    static member Parse =
        parseExact contentLocationP

    static member TryParse =
        parseOption contentLocationP

    override x.ToString () =
        ContentLocation.Format x

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

let internal methodF =
    function | CONNECT -> append "CONNECT"
             | DELETE -> append "DELETE" 
             | HEAD -> append "HEAD" 
             | GET -> append "GET" 
             | OPTIONS -> append "OPTIONS"
             | POST -> append "POST" 
             | PUT -> append "PUT"  
             | TRACE -> append "TRACE"
             | Method.Custom x -> append x

let internal methodP =
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

type Method with

    static member Format =
        format methodF

    static member Parse =
        parseExact methodP

    override x.ToString () =
        Method.Format x

(* Expect

   Taken from RFC 7231, Section 5.1.1 Expect
   See [http://tools.ietf.org/html/rfc7231#section-5.1.1] *)

type Expect =
    | Expect of Continue

and Continue =
    | Continue

let private expectF =
    function | Expect Continue -> append "100-continue"

let private expectP =
    skipStringCI "100-continue" >>% Expect Continue

type Expect with

    static member Format =
        format expectF

    static member Parse =
        parseExact expectP

    static member TryParse =
        parseOption expectP

    override x.ToString () =
        Expect.Format x

(* Max-Forwards

   Taken from RFC 7231, Section 5.1.2. Max-Forwards
   [http://tools.ietf.org/html/rfc7231#section-5.1.2] *)

type MaxForwards =
    | MaxForwards of int

let private maxForwardsF =
    function | MaxForwards x -> append (string x)

let private maxForwardsP =
    puint32 |>> (int >> MaxForwards)

type MaxForwards with

    static member Format =
        format maxForwardsF

    static member Parse =
        parseExact maxForwardsP

    static member TryParse =
        parseOption maxForwardsP

    override x.ToString () =
        MaxForwards.Format x

(* Quality Values

   Taken from RFC 7231, Section 5.3.1. Quality Values
   [http://tools.ietf.org/html/rfc7231#section-5.3.1] *)

let private weightF =
    function | Some (x: float) -> appendf1 ";q={0:G4}" x
             | _ -> id

let private valueOrDefault =
    function | Some x -> float (sprintf "0.%s" x)
             | _ -> 0.

let private d3P =
        manyMinMaxSatisfy 0 3 (fun c -> Set.contains c Grammar.digit) 
    .>> notFollowedBy (skipSatisfy ((?>) Grammar.digit))

let private d03P =
        skipManyMinMaxSatisfy 0 3 ((=) '0') 
    .>> notFollowedBy (skipSatisfy ((?>) Grammar.digit))

let private qvalueP =
    choice [ 
        skipChar '0' >>. opt (skipChar '.' >>. d3P) |>> valueOrDefault
        skipChar '1' >>. optional (skipChar '.' >>. d03P) >>% 1. ]

let private weightP =
    semicolonP >>. owsP >>. skipStringCI "q=" >>. qvalueP .>> owsP

(* Accept

   Taken from RFC 7231, Section 5.3.2. Accept
   [http://tools.ietf.org/html/rfc7231#section-5.3.2] *)

type Accept =
    | Accept of AcceptableMedia list

and AcceptableMedia =
    { MediaRange: MediaRange
      Parameters: AcceptParameters option }

and MediaRange =
    | Closed of Type * SubType * Parameters
    | Partial of Type * Parameters
    | Open of Parameters

and AcceptParameters =
    { Weight: float
      Extensions: AcceptExtensions }

and AcceptExtensions =
    Map<string, string option>

(* Formatting *)

let private mediaRangeF =
    function | MediaRange.Closed (Type x, SubType y, p) -> appendf2 "{0}/{1}" x y >> parametersF p
             | MediaRange.Partial (Type x, p) -> appendf1 "{0}/*" x >> parametersF p
             | MediaRange.Open p -> append "*/*" >> parametersF p

// TODO: Proper extensions...

let private acceptExtensionsF =
    function | (x: Map<string, string option>) when Map.isEmpty x -> id
             | _ -> id

let private acceptParametersF =
    function | Some { Weight = w; Extensions = e } -> weightF (Some w) >> acceptExtensionsF e
             | _ -> id

let private acceptableMediaF =
    function | { MediaRange = m
                 Parameters = p } -> mediaRangeF m >> acceptParametersF p

let private acceptF =
    function | Accept x -> join acceptableMediaF commaF x

(* Parsing *)

let private acceptExtP =
    tokenP .>>. opt (skipChar '=' >>. (quotedStringP <|> tokenP))

let private acceptExtsP =
    prefixP acceptExtP semicolonP |>> Map.ofList

let private acceptParamsP =
    weightP .>> owsP .>>. acceptExtsP
    |>> (fun (weight, extensions) ->
        { Weight = weight
          Extensions = extensions })

let private mediaRangeParameterP =
    notFollowedBy (owsP >>. skipStringCI "q=") >>. tokenP .>> skipChar '=' .>>. tokenP

let private mediaRangeParametersP =
    prefixP mediaRangeParameterP semicolonP |>> Map.ofList

let private openMediaRangeP = 
    skipString "*/*" >>. owsP >>. mediaRangeParametersP |>> MediaRange.Open

let private partialMediaRangeP = 
    tokenP .>> skipString "/*" .>> owsP .>>. mediaRangeParametersP
    |>> fun (x, parameters) -> 
            MediaRange.Partial (Type x, parameters)

let private closedMediaRangeP = 
    tokenP .>> skipChar '/' .>>. tokenP .>> owsP .>>. mediaRangeParametersP
    |>> fun ((x, y), parameters) -> 
            MediaRange.Closed (Type x, SubType y, parameters)

let private mediaRangeP = 
    choice [
        attempt openMediaRangeP
        attempt partialMediaRangeP
        closedMediaRangeP ]

let private acceptableMediaP = 
    mediaRangeP .>>. opt acceptParamsP
    |>> (fun (mediaRangeSpec, parameters) ->
            { MediaRange = mediaRangeSpec
              Parameters = parameters })

let private acceptP =
    infixP acceptableMediaP commaP |>> Accept

(* Augmentation *)

type Accept with

    static member Format =
        format acceptF

    static member Parse =
        parseExact acceptP

    static member TryParse =
        parseOption acceptP

    override x.ToString () =
        Accept.Format x

(* Accept-Charset

   Taken from RFC 7231, Section 5.3.3 Accept-Charset
   [http://tools.ietf.org/html/rfc7231#section-5.3.3] *)

type AcceptCharset =
    | AcceptCharset of AcceptableCharset list

and AcceptableCharset =
    { Charset: CharsetRange
      Weight: float option }

and CharsetRange =
    | Charset of Charset
    | Any

and Charset =
    | Charset of string

(* Presets *)

type Charset with

    /// Convenience definition for "iso-8859-1"
    static member Iso88591 =
        Charset "iso-8859-1"

    /// Convenience definition for "unicode-1-1"
    static member Unicode =
        Charset "unicode-1-1"

    /// Convenience definition for "utf-8"
    static member UTF8 =
        Charset "utf-8"

(* Formatting *)

let private charsetRangeF =
    function | CharsetRange.Charset (Charset x) -> append x
             | Any -> append "*"

let private acceptableCharsetF =
    function | { Charset = charset
                 Weight = weight } -> charsetRangeF charset >> weightF weight

let private acceptCharsetF =
    function | AcceptCharset x -> join acceptableCharsetF commaF x

(* Parsing *)

let private charsetRangeAnyP =
    skipChar '*' >>% CharsetRange.Any

let private charsetRangeCharsetP =
    tokenP |>> fun s -> CharsetRange.Charset (Charset s)

let private charsetRangeP = 
    choice [
        attempt charsetRangeAnyP
        charsetRangeCharsetP ]

let private acceptCharsetP =
    infix1P (charsetRangeP .>> owsP .>>. opt weightP) commaP
    |>> (List.map (fun (charsetRange, weight) ->
        { Charset = charsetRange
          Weight = weight }) >> AcceptCharset)

(* Augmentation *)

type AcceptCharset with

    static member Format =
        format acceptCharsetF

    static member Parse =
        parseExact acceptCharsetP

    static member TryParse =
        parseOption acceptCharsetP

    override x.ToString () =
        AcceptCharset.Format x

(* Accept-Encoding

   Taken from RFC 7231, Section 5.3.4. Accept-Encoding
   [http://tools.ietf.org/html/rfc7231#section-5.3.4] *)

type AcceptEncoding =
    | AcceptEncoding of AcceptableEncoding list

and AcceptableEncoding =
    { Encoding: EncodingRange
      Weight: float option }

and EncodingRange =
    | Coding of ContentCoding
    | Identity
    | Any

(* Formatting *)

let private encodingRangeF =
    function | Coding (ContentCoding x) -> append x
             | Identity -> append "identity"
             | Any -> append "*" 

let private acceptableEncodingF =
    function | { Encoding = e
                 Weight = w } -> encodingRangeF e >> weightF w

let private acceptEncodingF =
    function | AcceptEncoding x -> join acceptableEncodingF commaF x

(* Parsing *)

let private encodingRangeAnyP =
    skipChar '*' >>% Any

let private encodingRangeIdentityP =
    skipStringCI "identity" >>% Identity

let private encodingRangeCodingP =
    tokenP |>> fun s -> Coding (ContentCoding s)

let private encodingRangeP =
    choice [
        attempt encodingRangeAnyP
        attempt encodingRangeIdentityP
        encodingRangeCodingP ]

let private acceptEncodingP =
    infixP (encodingRangeP .>> owsP .>>. opt weightP) commaP
    |>> (List.map (fun (encoding, weight) ->
        { Encoding = encoding
          Weight = weight }) >> AcceptEncoding)

(* Augmentation *)

type AcceptEncoding with

    static member Format =
        format acceptEncodingF

    static member Parse =
        parseExact acceptEncodingP

    static member TryParse =
        parseOption acceptEncodingP

    override x.ToString () =
        AcceptEncoding.Format x

(* Accept-Language

   Taken from RFC 7231, Section 5.3.5. Accept-Language
   [http://tools.ietf.org/html/rfc7231#section-5.3.5] *)

type AcceptLanguage =
    | AcceptLanguage of AcceptableLanguage list

and AcceptableLanguage =
    { Language: LanguageRange
      Weight: float option }

(* Formatting *)

let private acceptableLanguageF x =
    languageRangeF x.Language >> weightF x.Weight

let private acceptLanguageF =
    function | AcceptLanguage x -> join acceptableLanguageF commaF x

(* Parsing *)

let private acceptLanguageP =
    infixP (languageRangeP .>> owsP .>>. opt weightP) commaP
    |>> (List.map (fun (languageRange, weight) ->
        { Language = languageRange
          Weight = weight }) >> AcceptLanguage)

(* Augmentation *)

type AcceptLanguage with

    static member Format =
        format acceptLanguageF

    static member Parse =
        parseExact acceptLanguageP

    static member TryParse =
        parseOption acceptLanguageP

    override x.ToString () =
        AcceptLanguage.Format x

(* Referer

   Taken from RFC 7231, Section 5.5.2 Referer
   [http://tools.ietf.org/html/rfc7231#section-5.5.2] *)

type Referer =
    | Referer of RefererUri
    
and RefererUri =
    | Absolute of AbsoluteUri
    | Partial of PartialUri

let private refererF =
    function | Referer (Absolute x) -> absoluteUriF x
             | Referer (Partial x) -> partialUriF x

let private refererP =
    choice [
        attempt absoluteUriP |>> (Absolute >> Referer)
        partialUriP |>> (Partial >> Referer) ]

type Referer with

    static member Format =
        format refererF

    static member Parse =
        parseExact refererP

    static member TryParse =
        parseOption refererP

    override x.ToString () =
        Referer.Format x

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

let private dateF =
    function | Date.Date x -> append (x.ToString "r")

let private dateP =
    httpDateP |>> Date.Date

type Date with

    static member Format =
        format dateF

    static member Parse =
        parseExact dateP

    static member TryParse =
        parseOption dateP

    override x.ToString () =
        Date.Format x

(* Location

   Taken from RFC 7231, Section 7.1.2 Location
   [http://tools.ietf.org/html/rfc7231#section-7.1.2] *)

type Location =
    | Location of UriReference

let private locationF =
    function | Location x -> uriReferenceF x

let private locationP =
    uriReferenceP |>> Location

type Location with

    static member Format =
        format locationF

    static member Parse =
        parseExact locationP

    static member TryParse =
        parseOption locationP

    override x.ToString () =
        Location.Format x

(* Retry-After

   Taken from RFC 7231, Section 7.1.3. Retry-After
   [http://tools.ietf.org/html/rfc7231#section-7.1.3] *)

type RetryAfter =
    | RetryAfter of RetryAfterChoice

and RetryAfterChoice =
    | Date of DateTime
    | Delay of TimeSpan

let private retryAfterF =
    function | RetryAfter (Date x) -> append (x.ToString "r")
             | RetryAfter (Delay x) -> append (string (int x.TotalSeconds))

let private retryAfterP =
    choice [
        attempt httpDateP |>> (Date >> RetryAfter)
        puint32 |>> (float >> TimeSpan.FromSeconds >> Delay >> RetryAfter) ]

type RetryAfter with

    static member Format =
        format retryAfterF

    static member Parse =
        parseExact retryAfterP

    static member TryParse =
        parseOption retryAfterP

    override x.ToString () =
        RetryAfter.Format x

(* Allow

   Taken from RFC 7231, Section 7.4.1 Allow
   [http://tools.ietf.org/html/rfc7231#section-7.4.1] *)

type Allow =
    | Allow of Method list

let private allowF =
    function | Allow x -> join methodF commaF x

let private allowP =
    infixP methodP commaP |>> Allow

type Allow with

    static member Format =
        format allowF

    static member Parse =
        parseExact allowP

    static member TryParse =
        parseOption allowP

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

let private lastModifiedF =
    function | LastModified x -> append (x.ToString "r")

let private lastModifiedP =
    httpDateP |>> LastModified

type LastModified with

    static member Format =
        format lastModifiedF

    static member Parse =
        parseExact lastModifiedP

    static member TryParse =
        parseOption lastModifiedP

    override x.ToString () =
        LastModified.Format x

(* ETag

   Taken from RFC 7232 Section 2.3 ETag
   [http://tools.ietf.org/html/rfc7232#section-2.3] *)

(* Entity Tag *)

type EntityTag =
    | Strong of string
    | Weak of string

let internal entityTagF =
    function | Strong x -> appendf1 "\"{0}\"" x
             | Weak x -> appendf1 "W/\"{0}\"" x

let private eTagChars =
    Set.unionMany [
        set [ char 0x21 ]
        charRange 0x23 0x7e
        obsText ]

let private opaqueTagP =
    skipChar Grammar.dquote >>. manySatisfy ((?>) eTagChars) .>> skipChar Grammar.dquote

let internal entityTagP =
    choice [
        attempt (skipString "W/" >>. opaqueTagP |>> Weak)
        opaqueTagP |>> Strong ]

(* ETag *)

type ETag =
    | ETag of EntityTag

let private eTagF =
    function | ETag x -> entityTagF x

let private eTagP =
    entityTagP |>> ETag

type ETag with

    static member Format =
        format eTagF

    static member Parse =
        parseExact eTagP

    static member TryParse =
        parseOption eTagP

    override x.ToString () =
        ETag.Format x

(* If-Match

   Taken from RFC 7232, Section 3.1, If-Match
   [http://tools.ietf.org/html/rfc7232#section-3.1] *)

type IfMatch =
    | IfMatch of IfMatchChoice

and IfMatchChoice =
    | EntityTags of EntityTag list
    | Any

let private ifMatchF =
    function | IfMatch (EntityTags x) -> join entityTagF commaF x
             | IfMatch (Any) -> append "*"

let private ifMatchP =
    choice [
        skipChar '*' >>% IfMatch (Any)
        infixP entityTagP commaP |>> (EntityTags >> IfMatch) ]

type IfMatch with

    static member Format =
        format ifMatchF

    static member Parse =
        parseExact ifMatchP

    static member TryParse =
        parseOption ifMatchP

    override x.ToString () =
        IfMatch.Format x

(* If-None-Match

   Taken from RFC 7232, Section 3.2, If-None-Match
   [http://tools.ietf.org/html/rfc7232#section-3.2] *)

type IfNoneMatch =
    | IfNoneMatch of IfNoneMatchChoice

and IfNoneMatchChoice =
    | EntityTags of EntityTag list
    | Any

let private ifNoneMatchF =
    function | IfNoneMatch (EntityTags x) -> join entityTagF commaF x
             | IfNoneMatch (Any) -> append "*"

let private ifNoneMatchP =
    choice [
        skipChar '*' >>% IfNoneMatch (Any)
        infixP entityTagP commaP |>> (EntityTags >> IfNoneMatch) ]

type IfNoneMatch with

    static member Format =
        format ifNoneMatchF

    static member Parse =
        parseExact ifNoneMatchP

    static member TryParse =
        parseOption ifNoneMatchP

    override x.ToString () =
        IfNoneMatch.Format x

(* If-Modified-Since

   Taken from RFC 7232, Section 3.3, If-Modified-Since
   [http://tools.ietf.org/html/rfc7232#section-3.3] *)

type IfModifiedSince =
    | IfModifiedSince of DateTime

let private ifModifiedSinceF =
    function | IfModifiedSince x -> append (x.ToString "r")

let private ifModifiedSinceP =
    httpDateP |>> IfModifiedSince

type IfModifiedSince with

    static member Format =
        format ifModifiedSinceF

    static member Parse =
        parseExact ifModifiedSinceP

    static member TryParse =
        parseOption ifModifiedSinceP

    override x.ToString () =
        IfModifiedSince.Format x

(* If-Unmodified-Since

   Taken from RFC 7232, Section 3.4, If-Unmodified-Since
   [http://tools.ietf.org/html/rfc7232#section-3.4] *)

type IfUnmodifiedSince =
    | IfUnmodifiedSince of DateTime

let private ifUnmodifiedSinceF =
    function | IfUnmodifiedSince x -> append (x.ToString "r")

let private ifUnmodifiedSinceP =
    httpDateP |>> IfUnmodifiedSince

type IfUnmodifiedSince with

    static member Format =
        format ifUnmodifiedSinceF

    static member Parse =
        parseExact ifUnmodifiedSinceP

    static member TryParse =
        parseOption ifUnmodifiedSinceP

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

and IfRangeChoice =
    | Date of DateTime
    | EntityTag of EntityTag

let private ifRangeF =
    function | IfRange (Date x) -> append (x.ToString "r")
             | IfRange (EntityTag x) -> entityTagF x

let private ifRangeP =
    (entityTagP |>> (EntityTag >> IfRange)) <|> (httpDateP |>> (Date >> IfRange))

type IfRange with

    static member Format =
        format ifRangeF

    static member Parse =
        parseExact ifRangeP

    static member TryParse =
        parseOption ifRangeP

    override x.ToString () =
        IfRange.Format x

(* RFC 7234

   Types, parsers and formatters implemented to mirror the semantics of
   HTTP as defined in RFC 7234.

   See [http://tools.ietf.org/html/rfc7234] *)

(* Age

   Taken from RFC 7234 Section 5.1 Age
   See [http://tools.ietf.org/html/rfc7234#section-5.1] *)

type Age =
    | Age of TimeSpan

let private ageF =
    function | Age x -> append (string x.TotalSeconds)

let private ageP =
    puint32 |>> (float >> TimeSpan.FromSeconds >> Age)

type Age with

    static member Format =
        format ageF

    static member Parse =
        parseExact ageP

    static member TryParse =
        parseOption ageP

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

(* Formatting *)

let private cacheDirectiveF =
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

let private cacheControlF =
    function | CacheControl x -> join cacheDirectiveF commaF x

(* Parsing *)

// TODO: Custom Directive Parsing

let private cacheDirectiveP =
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

let private cacheControlP =
    infix1P cacheDirectiveP commaP |>> CacheControl

(* Augmentation *)

type CacheControl with

    static member Format =
        format cacheControlF

    static member Parse =
        parseExact cacheControlP

    static member TryParse =
        parseOption cacheControlP

    override x.ToString () =
        CacheControl.Format x

(* Expires

   Taken from RFC 7234 Section 5.3 Expires
   See [http://tools.ietf.org/html/rfc7234#section-5.3] *)

type Expires =
    | Expires of DateTime

let private expiresF =
    function | Expires x -> append (x.ToString "r")

let private expiresP =
    httpDateP |>> Expires

type Expires with

    static member Format =
        format expiresF

    static member Parse =
        parseExact expiresP

    static member TryParse =
        parseOption expiresP

    override x.ToString () =
        Expires.Format x

(* RFC 7235

   Types, parsers and formatters implemented to mirror the semantics of
   HTTP as defined in RFC 7235.
   
   See [http://tools.ietf.org/html/rfc7235] *)

(* Access Authentication Framework
    
   Taken from RFC 7235, Section 2. Access Authentication Framework
   [http://tools.ietf.org/html/rfc7235#section-2] *)

type Credentials =
    | Token of AuthorizationScheme * string
    | Parameters of AuthorizationScheme * Map<string, string>

and AuthorizationScheme =
    | AuthorizationScheme of string

(* Formatting *)

let private authorizationSchemeF =
    function | AuthorizationScheme x -> append x

let private credentialsF =
    function | Token (x, y) -> authorizationSchemeF x >> append y
             | Parameters (x, y) -> authorizationSchemeF x >> parametersF y

(* Parsing *)

let private authorizationSchemeP =
    tokenP

let private authorizationParameterP =
    tokenP .>> bwsP .>> skipChar '=' .>>. (tokenP <|> quotedStringP)

// TODO: Parsers 

(* Authorization
    
   Taken from RFC 7235, Section 4.2. Authorization
   [http://tools.ietf.org/html/rfc7235#section-4.2] *)

type Authorization =
    | Authorization of Credentials list

let private authorizationF =
    function | Authorization x -> join credentialsF commaF x

// TODO: Parser

type Authorization with

    static member Format =
        format authorizationF

    override x.ToString () =
        Authorization.Format x