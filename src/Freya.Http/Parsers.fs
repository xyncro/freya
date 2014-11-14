module internal Freya.Http.Parsers

open System
open System.Globalization
open FParsec

(* Parsing *)

let parse p s =
    match run p s with
    | Success (x, _, _) -> x
    | Failure (e, _, _) -> failwith e

let parseP p s =
    match run p s with
    | Success (x, _, _) -> Some x
    | Failure (_, _, _) -> None

(* Character Ranges *)

let private charRange x y =
    set (List.map char [ x .. y ])

(* Operators *)

let private (?>) xs x =
    Set.contains x xs


module Generic =

    let scheme : Parser<Scheme, unit> =
        choice [
            skipStringCI "http" >>% HTTP
            skipStringCI "https" >>% HTTPS
            restOfLine false |>> Scheme.Custom ]

    let comma =
        skipChar ','

    let semicolon =
        skipChar ';'


module RFC5234 =

    (* Core Rules

       Taken from RFC 5234, Appendix B.1. Core Rules
       [http://tools.ietf.org/html/rfc5234#appendix-B.1] *)

    let alpha = 
        Set.unionMany [ 
            charRange 0x41 0x5a
            charRange 0x61 0x7a ]

    let digit = 
        charRange 0x30 0x39

    let dquote = 
        char 0x22

    let htab = 
        char 0x09

    let sp = 
        char 0x20

    let vchar =
        charRange 0x21 0x7e

    let wsp = 
        set [ sp; htab ]


module RFC7230 =

    open System
    open RFC5234

    (* Whitespace

       Taken from RFC 7230, Section 3.2.3. Whitespace
       [http://tools.ietf.org/html/rfc7230#section-3.2.3] *)
        
    let ows = 
        skipManySatisfy ((?>) wsp)

//    let rws =
//        skipMany1Satisfy (fun c -> Set.contains c wsp)

//    let bws =
//        ows

    (* Field Value Components

       Taken from RFC 7230, Section 3.2.6. Field Value Components
       [http://tools.ietf.org/html/rfc7230#section-3.2.6] *)

    let tchar = 
        Set.unionMany [ 
            set [ '!'; '#'; '$'; '%'; '&'; '\''; '*'
                  '+'; '-'; '.'; '^'; '_'; '`'; '|'; '~' ]
            alpha
            digit ]

    let token = 
        many1Satisfy ((?>) tchar)

    let obsText =
        charRange 0x80 0xff

    let qdtext =
        Set.unionMany [
            set [ htab; sp; char 0x21 ]
            charRange 0x23 0x5b
            charRange 0x5d 0x7e
            obsText ]

//    let ctext =
//        Set.unionMany [
//            set [ htab; sp ]
//            charRange 0x21 0x27
//            charRange 0x2a 0x5b
//            charRange 0x5d 0x7e
//            obsText ]

    let private quotedPairChars =
        Set.unionMany [
            set [ htab; sp ]
            vchar
            obsText ]

    let quotedPair =
            skipChar '\\' 
        >>. satisfy ((?>) quotedPairChars)

    let quotedString =
            skipChar dquote 
        >>. many (quotedPair <|> satisfy ((?>) qdtext)) |>> (fun x -> string (String (List.toArray x)))
        .>> skipChar dquote

    (* ABNF List Extension: #rule

       Taken from RFC 7230, Section 7. ABNF List Extension: #rule
       [http://tools.ietf.org/html/rfc7230#section-7] *)

    let private infixHead s p =
        (attempt p |>> Some) <|> (s >>% None)

    let private infixTail s p =
        many (ows >>? s >>? ows >>? opt p)

    (* Note:
       The infix and prefix parsers are designed to convey as accurately as possible the 
       meaning of the ABNF #rule extension including the laxity of specification for backward 
       compatibility. Whether they are a perfectly true representation is open to debate, 
       but they should perform sensibly under normal conditions. *)

    let infix s p = 
        infixHead s p .>>. infixTail s p .>> ows |>> fun (x, xs) -> x :: xs |> List.choose id

    let infix1 s p =
        notEmpty (infix s p)

    let prefix s p =
        many (ows >>? s >>? ows >>? p)

    (* Method

       Taken from RFC 7230, Section 3.1 Request Line
       [http://tools.ietf.org/html/rfc7230#section-3.1] *)

    let meth =
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

    (* HTTP Version

       Taken from RFC 7230, Section 3.1 Request Line
       [http://tools.ietf.org/html/rfc7230#section-3.1] *)

    let httpVersion : Parser<HttpVersion, unit> =
        choice [
            skipString "HTTP/1.0" >>% HttpVersion.HTTP 1.0
            skipString "HTTP/1.1" >>% HttpVersion.HTTP 1.1
            restOfLine false |>> HttpVersion.Custom ]

    (* Content-Length
        
       Taken from RFC 7230, Section 3.3.2 Content-Length
       [http://tools.ietf.org/html/rfc7230#section-3.3.2] *)

    let contentLength : Parser<ContentLength, unit> =
        puint32
        |>> (int >> ContentLength)

    (* Connection
        
       Taken from RFC 7230, Section 6.1 Connection
       [http://tools.ietf.org/html/rfc7230#section-6.1] *)

    let connection : Parser<Connection, unit> =
        infix1 (skipChar ',') token
        |>> (List.map ConnectionOption >> Connection)


module RFC7231 =

    open Generic
    open RFC5234
    open RFC7230

    (* Media-Type

       Includes the common definition of parameter as defined within this
       section, but applicable to multiple later types.

       Taken from RFC 7231, Section 3.1.1.1 Media-Type
       [http://tools.ietf.org/html/rfc7231#section-3.1.1.1] *)

    let private parameter =
        token .>> skipChar '=' .>>. (quotedString <|> token)

    let parameters =
        prefix semicolon parameter
        |>> Map.ofList

    let mediaType =
        token .>> skipChar '/' .>>. token .>>. parameters
        |>> (fun ((x, y), p) -> MediaType (Types.Type x, SubType y, p))

    (* Content-Type

       Taken from RFC 7231, Section 3.1.1.5 Content-Type
       [http://tools.ietf.org/html/rfc7231#section-3.1.1.5] *)

    let contentType =
        mediaType
        |>> ContentType

    (* Content-Encoding

       Taken from RFC 7231, Section 3.1.2.2 Content-Encoding
       [http://tools.ietf.org/html/rfc7231#section-3.1.2.2] *)

    let contentEncoding =
        infix1 comma token
        |>> (List.map Encoding >> ContentEncoding)

    (* Max-Forwards *)

    let maxForwards : Parser<MaxForwards, unit> =
        puint32
        |>> (int >> MaxForwards)

    // TODO: Proper Query String Parser
    let parseQuery =
        function | "" -> Map.empty
                 | s ->
                     s.Split [| '&' |]
                     |> Array.map (fun x -> x.Split [| '=' |])
                     |> Array.map (fun x -> x.[0], x.[1])
                     |> Map.ofArray

    (* Quality Values

       Taken from RFC 7231, Section 5.3.1. Quality Values
       [http://tools.ietf.org/html/rfc7231#section-5.3.1] *)

    let private valueOrDefault =
        function | Some x -> float (sprintf "0.%s" x)
                 | _ -> 0.

    let private d3 =
            manyMinMaxSatisfy 0 3 (fun c -> Set.contains c digit) 
        .>> notFollowedBy (skipSatisfy ((?>) digit))

    let private d03 =
            skipManyMinMaxSatisfy 0 3 ((=) '0') 
        .>> notFollowedBy (skipSatisfy ((?>) digit))

    let private qvalue =
        choice [ 
            skipChar '0' >>. opt (skipChar '.' >>. d3) |>> valueOrDefault
            skipChar '1' >>. optional (skipChar '.' >>. d03) >>% 1. ]

    let weight =
        semicolon >>. ows >>. skipStringCI "q=" >>. qvalue .>> ows

    (* Accept

       Taken from RFC 7231, Section 5.3.2. Accept
       [http://tools.ietf.org/html/rfc7231#section-5.3.2] *)

    // TODO: Test this quoted string implementation...

    let private acceptExt =
        token .>>. opt (skipChar '=' >>. (quotedString <|> token))

    let private acceptExts =
        prefix (skipChar ';') acceptExt
        |>> Map.ofList

    let private acceptParams =
        weight .>> ows .>>. acceptExts
        |>> (fun (weight, extensions) ->
            { Weight = weight
              Extensions = extensions })

    let private mediaRangeParameter =
        notFollowedBy (ows >>. skipStringCI "q=") >>. token .>> skipChar '=' .>>. token

    let private mediaRangeParameters =
        prefix semicolon mediaRangeParameter 
        |>> Map.ofList

    let private openMediaRange = 
        skipString "*/*" >>. ows >>. mediaRangeParameters
        |>> fun parameters -> 
                MediaRange.Open parameters

    let private partialMediaRange = 
        token .>> skipString "/*" .>> ows .>>. mediaRangeParameters
        |>> fun (x, parameters) -> 
                MediaRange.Partial (Types.Type x, parameters)

    let private closedMediaRange = 
        token .>> skipChar '/' .>>. token .>> ows .>>. mediaRangeParameters
        |>> fun ((x, y), parameters) -> 
                MediaRange.Closed (Types.Type x, SubType y, parameters)

    let private mediaRange = 
        choice [
            attempt openMediaRange
            attempt partialMediaRange
            closedMediaRange ]

    let private acceptableMedia : Parser<AcceptableMedia, unit> = 
        mediaRange .>>. opt acceptParams
        |>> (fun (mediaRangeSpec, parameters) ->
                { MediaRange = mediaRangeSpec
                  Parameters = parameters })

    let accept =
        infix comma acceptableMedia
        |>> Accept

    (* Accept-Charset

       Taken from RFC 7231, Section 5.3.3. Accept-Charset
       [http://tools.ietf.org/html/rfc7231#section-5.3.3] *)

    let private charsetSpecAny =
        skipChar '*' 
        >>% CharsetSpec.Any

    let private charsetSpecCharset =
        token
        |>> fun s -> CharsetSpec.Charset (Charset s)

    let private charsetSpec = 
        choice [
            attempt charsetSpecAny
            charsetSpecCharset ]

    let acceptCharset =
        infix1 (skipChar ',') (charsetSpec .>> ows .>>. opt weight)
        |>> (List.map (fun (charsetSpec, weight) ->
            { Charset = charsetSpec
              Weight = weight }) >> AcceptCharset)

    (* Accept-Encoding

       Taken from RFC 7231, Section 5.3.4. Accept-Encoding
       [http://tools.ietf.org/html/rfc7231#section-5.3.4] *)

    let private encodingSpecAny =
        skipChar '*'
        >>% EncodingSpec.Any

    let private encodingSpecIdentity =
        skipStringCI "identity"
        |>> fun _ -> EncodingSpec.Identity

    let private encodingSpecEncoding =
        token
        |>> fun s -> EncodingSpec.Encoding (Encoding s)

    let private encoding =
        choice [
            attempt encodingSpecAny
            attempt encodingSpecIdentity
            encodingSpecEncoding ]

    let acceptEncoding =
        infix comma (encoding .>> ows .>>. opt weight)
        |>> (List.map (fun (encoding, weight) ->
            { Encoding = encoding
              Weight = weight }) >> AcceptEncoding)

    (* Accept-Language

       Taken from RFC 7231, Section 5.3.5. Accept-Language
       [http://tools.ietf.org/html/rfc7231#section-5.3.5] *)

    (* Note: Language range taken as the Basic Language Range
       definition from RFC 4647, Section 3.1.3.1 *)

    let private languageRangeComponent =
        manyMinMaxSatisfy 1 8 (fun c -> Set.contains c alpha)

    let private languageRange =
        languageRangeComponent .>>. opt (skipChar '-' >>. languageRangeComponent)
        |>> function 
            | range, Some sub -> CultureInfo (sprintf "%s-%s" range sub)
            | range, _ -> CultureInfo (range)

    let acceptLanguage =
        infix comma (languageRange .>> ows .>>. opt weight)
        |>> (List.map (fun (languageRange, weight) ->
            { Language = languageRange
              Weight = weight }) >> AcceptLanguage)

    (* HTTP-Date

       Taken from RFC 7231, Section 7.1.1 HTTP-Date *)

    let private dateTimeFormat =
        CultureInfo.InvariantCulture.DateTimeFormat

    let private dateTimeAdjustment =
        DateTimeStyles.AdjustToUniversal

    let httpDate : Parser<DateTime, unit> =
        restOfLine false >>= (fun s ->
            match DateTime.TryParse (s, dateTimeFormat, dateTimeAdjustment) with
            | true, d -> preturn d
            | _ -> pzero)

    (* Date *)

    let date =
        httpDate
        |>> Date

    (* Allow

       Taken from RFC 7231, Section 7.4.1 Allow
       [http://tools.ietf.org/html/rfc7231#section-7.4.1] *)

    let allow =
        infix comma meth
        |>> Allow


module RFC7232 =

    open Generic
    open RFC5234
    open RFC7230
    open RFC7231

    let lastModified =
        httpDate
        |>> LastModified

    (* TODO: This is a naive formulation of an entity tag and does not
       properly support the grammar, particularly weak references, which
       should be implemented ASAP *)

    let eTag =
        skipChar dquote >>. token .>> skipChar dquote
        |>> Strong

    (* If-Match

       Taken from RFC 7232, Section 3.1, If-Match
       [http://tools.ietf.org/html/rfc7232#section-3.1] *)

    let ifMatch =
        choice [
            skipChar '*' >>% IfMatch.Any
            infix comma eTag |>> IfMatch.EntityTags ]

    (* If-None-Match

       Taken from RFC 7232, Section 3.2, If-None-Match
       [http://tools.ietf.org/html/rfc7232#section-3.2] *)

    let ifNoneMatch =
        choice [
            skipChar '*' >>% IfNoneMatch.Any
            infix comma eTag |>> IfNoneMatch.EntityTags ]

    let ifModifiedSince =
        httpDate
        |>> IfModifiedSince

    let ifUnmodifiedSince =
        httpDate
        |>> IfUnmodifiedSince


module RFC7234 =

    open RFC7231

    (* Age

       Taken from RFC 7234, Section 5.1, Age
       [http://tools.ietf.org/html/rfc7234#section-5.1] *)

    let age : Parser<Age, unit> =
        puint32
        |>> (float >> TimeSpan.FromSeconds >> Age)

    let expires =
        httpDate
        |>> Expires
