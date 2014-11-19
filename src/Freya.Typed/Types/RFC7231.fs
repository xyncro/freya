[<AutoOpen>]
module Freya.Typed.RFC7231

#nowarn "60"

open System
open System.Globalization
open FParsec

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
             | (x) -> append ";" >> join semicolonF pairF (Map.toList x |> List.rev)

let private parameterP =
    tokenP .>> skipChar '=' .>>. (quotedStringP <|> tokenP)

let private parametersP =
    prefixP semicolonP parameterP |>> Map.ofList

(* Media-Type *)

type MediaType =
    | MediaType of Type * SubType * Parameters

and Type =
    | Type of string

and SubType =
    | SubType of string

let private mediaTypeF =
    function | MediaType (Type x, SubType y, p) -> appendf2 "{0}/{1}" x y >> parametersF p

let private mediaTypeP =
    tokenP .>> slashP .>>. tokenP .>>. parametersP
    |>> (fun ((x, y), p) -> MediaType (Type x, SubType y, p))

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
    | ContentEncoding of Encoding list

and EncodingSpec =
    | Encoding of Encoding
    | Identity
    | Any

and Encoding =
    | Encoding of string

let private encodingF =
    function | Encoding x -> append x

let private contentEncodingF =
    function | ContentEncoding x -> join commaF encodingF x

let private contentEncodingP =
    infix1P commaP tokenP |>> (List.map Encoding >> ContentEncoding)

type ContentEncoding with

    static member Format =
        format contentEncodingF

    static member Parse =
        parseExact contentEncodingP

    static member TryParse =
        parseOption contentEncodingP

    override x.ToString () =
        ContentEncoding.Format x

(* Method

   Taken from RFC 7231, Section 4
   See [http://tools.ietf.org/html/rfc7231#section-4] *)

type Method =
    | DELETE 
    | HEAD 
    | GET 
    | OPTIONS 
    | PATCH 
    | POST 
    | PUT 
    | TRACE 
    | Custom of string

let private methodF =
    function | DELETE -> append "DELETE" 
             | HEAD -> append "HEAD" 
             | GET -> append "GET" 
             | OPTIONS -> append "OPTIONS"
             | PATCH -> append "PATCH" 
             | POST -> append "POST" 
             | PUT -> append "PUT"  
             | TRACE -> append "TRACE"
             | Method.Custom x -> append x

let private methodP =
    choice [
        skipStringCI "delete" >>% DELETE
        skipStringCI "head" >>% HEAD
        skipStringCI "get" >>% GET
        skipStringCI "options" >>% OPTIONS
        skipStringCI "patch" >>% PATCH
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
        manyMinMaxSatisfy 0 3 (fun c -> Set.contains c RFC5234.digit) 
    .>> notFollowedBy (skipSatisfy ((?>) RFC5234.digit))

let private d03P =
        skipManyMinMaxSatisfy 0 3 ((=) '0') 
    .>> notFollowedBy (skipSatisfy ((?>) RFC5234.digit))

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
    function | Some ({ Weight = w; Extensions = e }) -> weightF (Some w) >> acceptExtensionsF e
             | _ -> id

let private acceptableMediaF (value: AcceptableMedia) =
    mediaRangeF value.MediaRange >> acceptParametersF value.Parameters

let private acceptF =
    function | Accept x -> join commaF acceptableMediaF x

(* Parsing *)

let private acceptExtP =
    tokenP .>>. opt (skipChar '=' >>. (quotedStringP <|> tokenP))

let private acceptExtsP =
    prefixP semicolonP acceptExtP |>> Map.ofList

let private acceptParamsP =
    weightP .>> owsP .>>. acceptExtsP
    |>> (fun (weight, extensions) ->
        { Weight = weight
          Extensions = extensions })

let private mediaRangeParameterP =
    notFollowedBy (owsP >>. skipStringCI "q=") >>. tokenP .>> skipChar '=' .>>. tokenP

let private mediaRangeParametersP =
    prefixP semicolonP mediaRangeParameterP |>> Map.ofList

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
    infixP commaP acceptableMediaP |>> Accept

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
    { Charset: CharsetSpec
      Weight: float option }

and CharsetSpec =
    | Charset of Charset
    | Any

and Charset =
    | Charset of string

(* Formatting *)

let private charsetSpecF =
    function | CharsetSpec.Charset (Charset x) -> append x
             | CharsetSpec.Any -> append "*"

let private acceptableCharsetF (value: AcceptableCharset) =
    charsetSpecF value.Charset >> weightF value.Weight

let acceptCharsetF =
    function | AcceptCharset x -> join commaF acceptableCharsetF x

(* Parsing *)

let private charsetSpecAnyP =
    skipChar '*' >>% CharsetSpec.Any

let private charsetSpecCharsetP =
    tokenP |>> fun s -> CharsetSpec.Charset (Charset s)

let private charsetSpecP = 
    choice [
        attempt charsetSpecAnyP
        charsetSpecCharsetP ]

let private acceptCharsetP =
    infix1P commaP (charsetSpecP .>> owsP .>>. opt weightP)
    |>> (List.map (fun (charsetSpec, weight) ->
        { Charset = charsetSpec
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
    { Encoding: EncodingSpec
      Weight: float option }

(* Formatting *)

let private encodingSpecF =
    function | EncodingSpec.Encoding (Encoding.Encoding x) -> append x
             | EncodingSpec.Identity -> append "identity"
             | EncodingSpec.Any -> append "*" 

let private acceptableEncodingF x =
    encodingSpecF x.Encoding >> weightF x.Weight

let private acceptEncodingF =
    function | AcceptEncoding x -> join commaF acceptableEncodingF x

(* Parsing *)

let private encodingSpecAnyP =
    skipChar '*' >>% EncodingSpec.Any

let private encodingSpecIdentityP =
    skipStringCI "identity" >>% EncodingSpec.Identity

let private encodingSpecEncodingP =
    tokenP |>> fun s -> EncodingSpec.Encoding (Encoding s)

let private encodingP =
    choice [
        attempt encodingSpecAnyP
        attempt encodingSpecIdentityP
        encodingSpecEncodingP ]

let private acceptEncodingP =
    infixP commaP (encodingP .>> owsP .>>. opt weightP)
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
    { Language: CultureInfo
      Weight: float option }

(* Formatting *)

let private cultureInfoF (x: CultureInfo) =
    append x.Name

let private acceptableLanguageF x =
    cultureInfoF x.Language >> weightF x.Weight

let private acceptLanguageF =
    function | AcceptLanguage x -> join commaF acceptableLanguageF x

(* Parsing *)

(* Note: Language range taken as the Basic Language Range
    definition from RFC 4647, Section 3.1.3.1 *)

let private languageRangeComponentP =
    manyMinMaxSatisfy 1 8 ((?>) RFC5234.alpha)

let private languageRangeP =
    languageRangeComponentP .>>. opt (skipChar '-' >>. languageRangeComponentP)
    |>> function 
        | range, Some sub -> CultureInfo (sprintf "%s-%s" range sub)
        | range, _ -> CultureInfo (range)

let private acceptLanguageP =
    infixP commaP (languageRangeP .>> owsP .>>. opt weightP)
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

(* HTTP-Date

   Taken from RFC 7231, Section 7.1.1 HTTP-Date *)

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

(* Retry-After

   Taken from RFC 7231, Section 7.1.3. Retry-After
   [http://tools.ietf.org/html/rfc7231#section-7.1.3] *)

type RetryAfter =
    | Date of DateTime
    | Delay of int

let private retryAfterF =
    function | Date x -> append (x.ToString "r")
             | Delay x -> append (string x)

let private retryAfterP =
    choice [
        attempt httpDateP |>> Date
        puint32 |>> (int >> Delay) ]

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
    function | Allow x -> join commaF methodF x

let private allowP =
    infixP commaP methodP |>> Allow

type Allow with

    static member Format =
        format allowF

    static member Parse =
        parseExact allowP

    static member TryParse =
        parseOption allowP

    override x.ToString () =
        Allow.Format x