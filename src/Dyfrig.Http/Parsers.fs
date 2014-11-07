module internal Dyfrig.Http.Parsers

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
        >>. many (quotedPair <|> satisfy ((?>) qdtext)) |>> (fun x -> String (List.toArray x))
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

    let meth : Parser<Method, unit> =
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

    (* Connection
        
       Taken from RFC 7230, Section 6.1 Connection
       [http://tools.ietf.org/html/rfc7230#section-6.1] *)

    let connection : Parser<Connection, unit> =
        infix1 (skipChar ',') token
        |>> (List.map ConnectionOption >> Connection)


module RFC7231 =

    open RFC5234
    open RFC7230

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
        function
        | Some x -> float (sprintf "0.%s" x)
        | _ -> 0.

    let private d3 =
            manyMinMaxSatisfy 0 3 (fun c -> Set.contains c digit) 
        .>> notFollowedBy (skipSatisfy ((?>) digit))

    let private d03 =
            skipManyMinMaxSatisfy 0 3 ((=) '0') 
        .>> notFollowedBy (skipSatisfy ((?>) digit))

    let private qvalue =
        choice
            [ skipChar '0' >>. opt (skipChar '.' >>. d3) |>> valueOrDefault
              skipChar '1' >>. optional (skipChar '.' >>. d03) >>% 1. ]

    let weight =
        skipChar ';' >>. ows >>. skipStringCI "q=" >>. qvalue .>> ows

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

    let private parameter =
        notFollowedBy (ows >>. skipStringCI "q=") >>. token .>> skipChar '=' .>>. token

    let private parameters =
        prefix (skipChar ';') parameter 
        |>> Map.ofList

    let private mediaRangeSpecOpen = 
        skipString "*/*"
        |>> fun _ -> MediaRangeSpec.Open

    let private mediaRangeSpecPartial = 
        token .>> skipString "/*"
        |>> fun x -> MediaRangeSpec.Partial (Type x)

    let private mediaRangeSpecClosed = 
        token .>> skipChar '/' .>>. token
        |>> fun (x, y) -> MediaRangeSpec.Closed (Type x, SubType y)

    let private mediaRangeSpec = 
        choice [
            attempt mediaRangeSpecOpen
            attempt mediaRangeSpecPartial
            mediaRangeSpecClosed ]

    let private mediaRange : Parser<MediaRange, unit> = 
        mediaRangeSpec .>> ows .>>. parameters
        |>> (fun (mediaRangeSpec, parameters) ->
                { MediaRange = mediaRangeSpec
                  Parameters = parameters })

    let accept =
        infix (skipChar ',') (mediaRange .>> ows .>>. opt acceptParams)
        |>> (List.map (fun (mediaRange, acceptParams) ->
            let weight = 
                acceptParams 
                |> Option.map fst

            let parameters = 
                acceptParams 
                |> Option.map snd
                |> Option.getOrElse Map.empty

            { MediaRange = mediaRange
              Weight = weight
              Parameters = parameters }) >> Accept)

    (* Accept-Charset

       Taken from RFC 7231, Section 5.3.3. Accept-Charset
       [http://tools.ietf.org/html/rfc7231#section-5.3.3] *)

    let private charsetSpecAny =
        skipChar '*' 
        >>%  CharsetSpec.Any

    let private charsetSpecCharset =
        token
        |>> fun s -> CharsetSpec.Charset (Charset.Charset s)

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
        |>> fun s -> EncodingSpec.Encoding (Encoding.Encoding s)

    let private encoding =
        choice [
            attempt encodingSpecAny
            attempt encodingSpecIdentity
            encodingSpecEncoding ]

    let acceptEncoding =
        infix (skipChar ',') (encoding .>> ows .>>. opt weight)
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
        infix (skipChar ',') (languageRange .>> ows .>>. opt weight)
        |>> (List.map (fun (languageRange, weight) ->
            { Language = languageRange
              Weight = weight }) >> AcceptLanguage)


module RFC7232 =

    open RFC5234
    open RFC7230

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
            skipChar '*' |>> fun _ -> IfMatch.Any
            infix (skipChar ',') eTag |>> fun x ->  IfMatch.EntityTags x ]

    (* If-None-Match

       Taken from RFC 7232, Section 3.2, If-None-Match
       [http://tools.ietf.org/html/rfc7232#section-3.2] *)

    let ifNoneMatch =
        choice [
            skipChar '*' >>% IfNoneMatch.Any
            infix (skipChar ',') eTag |>> fun x -> IfNoneMatch.EntityTags x ]
