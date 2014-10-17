[<AutoOpen>]
module internal Dyfrig.Http.Parsers

open System.Globalization
open FParsec

(* Helpers *)

let private parse p s =
    match run p s with
    | Success (x, _, _) -> Some x
    | Failure (_, _, _) -> None

(* RFC 5234 *)

(* Core Rules

   Taken from RFC 5234, Appendix B.1. Core Rules
   [http://tools.ietf.org/html/rfc5234#appendix-B.1] *)

let private DQUOTE = 
    char 0x22

let private HTAB = 
    char 0x09

let private SP = 
    char 0x20

let private ALPHA = 
    Set.unionMany 
        [ set (List.map char [0x41 .. 0x5a])
          set (List.map char [0x61 .. 0x7a]) ]

let private DIGIT = 
    set (List.map char [0x30 .. 0x39])

let private WSP = 
    set [ SP; HTAB ]

let private isALPHA c = 
    Set.contains c ALPHA

let private isDIGIT c = 
    Set.contains c DIGIT

let private isWSP c = 
    Set.contains c WSP

(* RFC 7230 *)

(* Whitespace

   Taken from RFC 7230, Section 3.2.3. Whitespace
   [http://tools.ietf.org/html/rfc7230#section-3.2.3] *)
        
let private OWS = 
    skipManySatisfy isWSP

//let private RWS = 
//    skipMany1Satisfy isWSP

(* Field Value Components

   Taken from RFC 7230, Section 3.2.6. Field Value Components
   [http://tools.ietf.org/html/rfc7230#section-3.2.6] *)

let private nonDelimiters =
    set [ '!'; '#'; '$'; '%'; '&'; '\''; '*'
          '+'; '-'; '.'; '^'; '_'; '`'; '|'; '~' ]

let private TCHAR = 
    Set.unionMany 
        [ nonDelimiters
          ALPHA
          DIGIT ]

let private isTCHAR c =
    Set.contains c TCHAR

let private token = 
    many1Satisfy isTCHAR

(* ABNF List Extension: #rule

   Taken from RFC 7230, Section 7. ABNF List Extension: #rule
   [http://tools.ietf.org/html/rfc7230#section-7] *)

let private infixHead s p =
    (attempt p |>> Some) <|> (s >>% None)

let private infixTail s p =
    many (OWS >>? s >>? OWS >>? opt p)

(* Note:
   The infix and prefix parsers are designed to convey as accurately as possible the 
   meaning of the ABNF #rule extension including the laxity of specification for backward 
   compatibility. Whether they are a perfectly true representation is open to debate, 
   but they should perform sensibly under normal conditions. *)

let private infix s p = 
    infixHead s p .>>. infixTail s p .>> OWS |>> fun (x, xs) ->  x :: xs |> List.choose id

let private infix1 s p =
    notEmpty (infix s p)

let private prefix s p =
    many (OWS >>? s >>? OWS >>? p)

(* RFC 7231 *)

let parseMethod =
    function | "DELETE" -> DELETE 
             | "HEAD" -> HEAD 
             | "GET" -> GET 
             | "OPTIONS" -> OPTIONS
             | "PATCH" -> PATCH 
             | "POST" -> POST 
             | "PUT" -> PUT 
             | "TRACE" -> TRACE
             | x -> Method.Custom x

let parseProtocol =
    function | "HTTP/1.0" -> Protocol.HTTP 1.0 
             | "HTTP/1.1" -> Protocol.HTTP 1.1 
             | x -> Protocol.Custom x

let parseScheme =
    function | "http" -> HTTP 
             | "https" -> HTTPS 
             | x -> Scheme.Custom x

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
        manyMinMaxSatisfy 0 3 isDIGIT .>> notFollowedBy (skipSatisfy isDIGIT)

let private isZero =
    isAnyOf [ '0' ]

let private d03 =
    skipManyMinMaxSatisfy 0 3 isZero .>> notFollowedBy (skipSatisfy isDIGIT)

let private qvalue =
    choice
        [ skipChar '0' >>. opt (skipChar '.' >>. d3) |>> valueOrDefault
          skipChar '1' >>. optional (skipChar '.' >>. d03) >>% 1. ]

let weight =
    skipChar ';' >>. OWS >>. skipStringCI "q=" >>. qvalue .>> OWS

(* Accept

   Taken from RFC 7231, Section 5.3.2. Accept
   [http://tools.ietf.org/html/rfc7231#section-5.3.2] *)

// TODO: tokens are not the only values here, dquoted strings need implementing

let private acceptExt =
    token .>>. opt (skipChar '=' >>. token)

let private acceptExts =
    prefix (skipChar ';') acceptExt
    |>> Map.ofList

let private acceptParams =
    weight .>> OWS .>>. acceptExts

let private parameter =
    notFollowedBy (OWS >>. skipStringCI "q=") >>. token .>> skipChar '=' .>>. token

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
    mediaRangeSpec .>> OWS .>>. parameters
    |>> (fun (mediaRangeSpec, parameters) ->
            { MediaRange = mediaRangeSpec
              Parameters = parameters })

let private accept =
    infix (skipChar ',') (mediaRange .>> OWS .>>. opt acceptParams)
    |>> List.map (fun (mediaRange, acceptParams) ->
        let weight = 
            acceptParams 
            |> Option.map fst

        let parameters = 
            acceptParams 
            |> Option.map snd
            |> Option.getOrElse Map.empty

        { MediaRange = mediaRange
          Weight = weight
          Parameters = parameters })

let parseAccept =
    parse accept

(* Accept-Charset

   Taken from RFC 7231, Section 5.3.3. Accept-Charset
   [http://tools.ietf.org/html/rfc7231#section-5.3.3] *)

let private charsetSpecAny =
    skipChar '*'
    |>> fun _ -> CharsetSpec.Any

let private charsetSpecCharset =
    token
    |>> fun s -> CharsetSpec.Charset (Charset.Charset s)

let private charsetSpec = 
    choice [
        attempt charsetSpecAny
        charsetSpecCharset ]

let private acceptCharset =
    infix1 (skipChar ',') (charsetSpec .>> OWS .>>. opt weight)
    |>> List.map (fun (charsetSpec, weight) ->
        { Charset = charsetSpec
          Weight = weight })

let parseAcceptCharset =
    parse acceptCharset

(* Accept-Encoding

   Taken from RFC 7231, Section 5.3.4. Accept-Encoding
   [http://tools.ietf.org/html/rfc7231#section-5.3.4] *)

let private encodingSpecAny =
    skipChar '*'
    |>> fun _ -> EncodingSpec.Any

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

let private acceptEncoding =
    infix (skipChar ',') (encoding .>> OWS .>>. opt weight)
    |>> List.map (fun (encoding, weight) ->
        { Encoding = encoding
          Weight = weight })

let parseAcceptEncoding =
    parse acceptEncoding

(* Accept-Language

   Taken from RFC 7231, Section 5.3.5. Accept-Language
   [http://tools.ietf.org/html/rfc7231#section-5.3.5] *)

(* Note: Language range taken as the Basic Language Range
   definition from RFC 4647, Section 3.1.3.1 *)

let private languageRangeComponent =
    manyMinMaxSatisfy 1 8 isALPHA

let private languageRange =
    languageRangeComponent .>>. opt (skipChar '-' >>. languageRangeComponent)
    |>> function 
        | range, Some sub -> CultureInfo (sprintf "%s-%s" range sub)
        | range, _ -> CultureInfo (range)

let private acceptLanguage =
    infix (skipChar ',') (languageRange .>> OWS .>>. opt weight)
    |>> List.map (fun (languageRange, weight) ->
        { Language = languageRange
          Weight = weight })

let parseAcceptLanguage =
    parse acceptLanguage

(* RFC 7232 *)

(* TODO: This is a naive formulation of an entity tag and does not
   properly support the grammar, particularly weak references, which
   should be implemented ASAP *)

let private eTag =
    skipChar DQUOTE >>. token .>> skipChar DQUOTE
    |>> Strong

let parseETag =
    parse eTag

(* If-Match

   Taken from RFC 7232, Section 3.1, If-Match
   [http://tools.ietf.org/html/rfc7232#section-3.1] *)

let private ifMatch =
    choice [
        skipChar '*' |>> fun _ -> IfMatch.Any
        infix (skipChar ',') eTag |>> fun x ->  IfMatch.EntityTags x ]

let parseIfMatch =
    parse ifMatch

(* If-None-Match

   Taken from RFC 7232, Section 3.2, If-None-Match
   [http://tools.ietf.org/html/rfc7232#section-3.2] *)

let private ifNoneMatch =
    choice [
        skipChar '*' |>> fun _ -> IfNoneMatch.Any
        infix (skipChar ',') eTag |>> fun x -> IfNoneMatch.EntityTags x ]

let parseIfNoneMatch =
    parse ifNoneMatch