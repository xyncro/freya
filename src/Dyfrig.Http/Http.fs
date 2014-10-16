namespace Dyfrig.Http

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open Aether
open Aether.Operators
open Dyfrig.Core
open Dyfrig.Core.Operators

(* Content Negotiation
            
   Taken from RFC 7231, Section 5.3
   [http://tools.ietf.org/html/rfc7231#section-5.3] *)

(* Accept

   Taken from RFC 7231, Section 5.3.2. Accept
   [http://tools.ietf.org/html/rfc7231#section-5.3.2] *)

type Accept =
    { MediaRange: MediaRange
      MediaRangeParameters: Map<string, string>
      ExtensionParameters: Map<string, string option>
      Weight: float option }

and MediaRange =
    | Specified of SpecifiedMediaRange
    | Partial of PartialMediaRange

and SpecifiedMediaRange =
    | Closed of MediaType * MediaSubType

and PartialMediaRange =
    | Partial of MediaType
    | Open

and MediaType =
    | MediaType of string

and MediaSubType =
    | MediaSubType of string

(* Accept-Charset

   Taken from RFC 7231, Section 5.3.3. Accept-Charset
   [http://tools.ietf.org/html/rfc7231#section-5.3.3] *)

type AcceptCharset =
    { Charset: Charset
      Weight: float option }

and Charset =
    | Specified of SpecifiedCharset
    | Any

and SpecifiedCharset =
    | Named of string

(* Accept-Encoding

   Taken from RFC 7231, Section 5.3.4. Accept-Encoding
   [http://tools.ietf.org/html/rfc7231#section-5.3.4] *)

type AcceptEncoding =
    { Encoding: Encoding
      Weight: float option }

and Encoding =
    | Specified of SpecifiedEncoding
    | Any

and SpecifiedEncoding =
    | Named of string
    | Identity

(* Accept-Language

   Taken from RFC 7231, Section 5.3.5. Accept-Language
   [http://tools.ietf.org/html/rfc7231#section-5.3.5] *)

type AcceptLanguage =
    { Language: CultureInfo
      Weight: float option }

(* Precondition
            
   Taken from RFC 7232, Section 3
   [http://tools.ietf.org/html/rfc7232#section-3] *)

type EntityTag =
    | Strong of string
    | Weak of string

(* If-Match

   Taken from RFC 7232, Section 3.1, If-Match
   [http://tools.ietf.org/html/rfc7232#section-3.1] *)

type IfMatch =
    | EntityTags of EntityTag list
    | Any

(* If-None-Match

   Taken from RFC 7232, Section 3.2, If-None-Match
   [http://tools.ietf.org/html/rfc7232#section-3.2] *)

type IfNoneMatch =
    | EntityTags of EntityTag list
    | Any
    
(* Method
    
   Types representing the method of an HTTP request.
   See [http://tools.ietf.org/html/rfc7231] for details. *)

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

(* Protocol
    
   Types representing the protocol of an HTTP request.
   See [http://tools.ietf.org/html/rfc7231] for details. *)

type Protocol =
    | HTTP of float 
    | Custom of string

(* Scheme
    
   Types representing the scheme of an HTTP request.
   See [http://tools.ietf.org/html/rfc7231] for details. *)

type Scheme =
    | HTTP 
    | HTTPS 
    | Custom of string


[<RequireQualifiedAccess>]
module private Option =

    let getOrElse def =
        function | Some x -> x
                 | _ -> def


[<AutoOpen>]
module Negotiation =

    let private (==) s1 s2 =
        String.Equals (s1, s2, StringComparison.OrdinalIgnoreCase)

    let inline private weight (a: ^a) =
        ((^a) : (member Weight: float option) a)

    let inline private prepare requested =
        requested
        |> List.filter (fun r -> weight r <> Some 0.)
        |> List.sortBy (fun r -> weight r |> Option.getOrElse 1.)
        |> List.rev

    (* Content Negotiation
            
       Taken from RFC 7231, Section 5.3
       [http://tools.ietf.org/html/rfc7231#section-5.3] *)

    (* Accept

       Taken from RFC 7231, Section 5.3.2. Accept
       [http://tools.ietf.org/html/rfc7231#section-5.3.2] *)

    let private (|ClosedM|_|) =
        function | MediaRange.Specified (Closed (MediaType x, MediaSubType y)) -> Some (x, y)
                 | _ -> None

    let private (|PartialM|_|) =
        function | MediaRange.Partial (Partial (MediaType x)) -> Some x
                 | _ -> None

    let private (|OpenM|_|) =
        function | MediaRange.Partial (Open) -> Some ()
                 | _ -> None

    let private matchAccept (Closed (MediaType t, MediaSubType s)) =
        function | ClosedM (t', s') when t == t' && s == s' -> true
                 | PartialM t' when t == t' -> true
                 | OpenM _ -> true
                 | _ -> false

    let private scoreAccept (Closed (MediaType t, MediaSubType s)) =
        function | ClosedM (t', s') when t == t' && s == s' -> 3
                 | PartialM t' when t == t' -> 2
                 | OpenM _ -> 1
                 | _ -> 0

    let private selectAccept (available: SpecifiedMediaRange list) =
        List.map (fun r -> 
            available 
            |> List.filter (fun a -> matchAccept a r.MediaRange)
            |> List.sortBy (fun a -> scoreAccept a r.MediaRange)) >> List.concat

    let negotiateAccept (available: SpecifiedMediaRange list) =
        prepare >> selectAccept available

    (* Accept-Charset

       Taken from RFC 7231, Section 5.3.3. Accept-Charset
       [http://tools.ietf.org/html/rfc7231#section-5.3.3] *)

    let private (|NamedC|_|) =
        function | Charset.Specified (SpecifiedCharset.Named s) -> Some s
                 | _ -> None

    let private (|AnyC|_|) =
        function | Charset.Any -> Some ()
                 | _ -> None

    let private matchAcceptCharset =
        function | SpecifiedCharset.Named s, NamedC s' when s == s' -> true
                 | _, AnyC _ -> true
                 | _ -> false

    let private selectAcceptCharset (available: SpecifiedCharset list) =
        List.map (fun r ->
            available
            |> List.filter (fun a -> matchAcceptCharset (a, r.Charset))) >> List.concat

    let negotiateAcceptCharset (available: SpecifiedCharset list) =
        prepare >> selectAcceptCharset available

    (* Accept-Encoding

       Taken from RFC 7231, Section 5.3.4. Accept-Encoding
       [http://tools.ietf.org/html/rfc7231#section-5.3.4] *)

    let private (|NamedE|_|) =
        function | Encoding.Specified (SpecifiedEncoding.Named e) -> Some e
                 | _ -> None

    let private (|IdentityE|_|) =
        function | Encoding.Specified (SpecifiedEncoding.Identity) -> Some ()
                 | _ -> None

    let private (|AnyE|_|) =
        function | Encoding.Any -> Some ()
                 | _ -> None

    let private matchAcceptEncoding =
        function | SpecifiedEncoding.Named e, NamedE e' when e == e' -> true
                 | SpecifiedEncoding.Identity, IdentityE _ -> true
                 | _, AnyE _ -> true
                 | _ -> false

    let private selectAcceptEncoding (available: SpecifiedEncoding list) =
        List.map (fun r ->
            available
            |> List.filter (fun a -> matchAcceptEncoding (a, r.Encoding))) >> List.concat

    let negotiateAcceptEncoding (available: SpecifiedEncoding list) =
        prepare >> selectAcceptEncoding available

    (* Accept-Language

       Taken from RFC 7231, Section 5.3.5. Accept-Language
       [http://tools.ietf.org/html/rfc7231#section-5.3.5] *)

    let private matchAcceptLanguage (c: CultureInfo) =
        function | c' when c = c' || c.Parent = c' -> true
                 | _ -> false

    let private scoreAcceptLanguage (c: CultureInfo) =
        function | c' when c = c' -> 2
                 | c' when c.Parent = c' -> 1
                 | _ -> 0

    let private selectAcceptLanguage (available: CultureInfo list) =
        List.map (fun r -> 
            available 
            |> List.filter (fun a -> matchAcceptLanguage a r.Language)
            |> List.sortBy (fun a -> scoreAcceptLanguage a r.Language)) >> List.concat

    let negotiateAcceptLanguage (available: CultureInfo list) =
        prepare >> selectAcceptLanguage available


[<AutoOpen>]
module internal Parsing =

    open FParsec


    let parse p s =
        match run p s with
        | Success (x, _, _) -> Some x
        | Failure (_, _, _) -> None


    [<AutoOpen>]
    module RFC_5234 =

        [<AutoOpen>]
        module Appendix_B_1 =

            (* Core Rules

               Taken from RFC 5234, Appendix B.1. Core Rules
               [http://tools.ietf.org/html/rfc5234#appendix-B.1] *)

//            let CR = 
//                char 0x0d

            let DQUOTE = 
                char 0x22

            let HTAB = 
                char 0x09

            let SP = 
                char 0x20

            let ALPHA = 
                Set.unionMany 
                    [ set (List.map char [0x41 .. 0x5a])
                      set (List.map char [0x61 .. 0x7a]) ]

            let DIGIT = 
                set (List.map char [0x30 .. 0x39])

//            let VCHAR = 
//                set (List.map char [0x21 .. 0x7e])

            let WSP = 
                set [ SP; HTAB ]

            let isALPHA c = 
                Set.contains c ALPHA

            let isDIGIT c = 
                Set.contains c DIGIT

            let isWSP c = 
                Set.contains c WSP


    [<AutoOpen>]
    module RFC_7230 =

        [<AutoOpen>]
        module Section_3_2_3 =

            (* Whitespace

               Taken from RFC 7230, Section 3.2.3. Whitespace
               [http://tools.ietf.org/html/rfc7230#section-3.2.3] *)
        
            let OWS = 
                skipManySatisfy isWSP

    //        let RWS = 
    //            skipMany1Satisfy isWSP


        [<AutoOpen>]
        module Section_3_2_6 =

            (* Field Value Components

               Taken from RFC 7230, Section 3.2.6. Field Value Components
               [http://tools.ietf.org/html/rfc7230#section-3.2.6] *)

            let private nonDelimiters =
                set [ '!'; '#'; '$'; '%'; '&'; '\''; '*'
                      '+'; '-'; '.'; '^'; '_'; '`'; '|'; '~' ]

            let TCHAR = 
                Set.unionMany 
                    [ nonDelimiters
                      ALPHA
                      DIGIT ]

            let isTCHAR c =
                Set.contains c TCHAR

            let token = 
                many1Satisfy isTCHAR


        [<AutoOpen>]
        module Section_7 =

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

            let infix s p = 
                infixHead s p .>>. infixTail s p .>> OWS |>> fun (x, xs) ->  x :: xs |> List.choose id

            let infix1 s p =
                notEmpty (infix s p)

            let prefix s p =
                many (OWS >>? s >>? OWS >>? p)

//            let prefix1 s p =
//                notEmpty (prefix s p)


    [<AutoOpen>]
    module RFC_7231 =

        [<AutoOpen>]
        module Section_5_3_1 =

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


        [<AutoOpen>]
        module Section_5_3_2 =
        
            (* Accept

               Taken from RFC 7231, Section 5.3.2. Accept
               [http://tools.ietf.org/html/rfc7231#section-5.3.2] *)

            // TODO: tokens are not the only values here, dquoted strings need implementing

            let private acceptExtension =
                token .>>. opt (skipChar '=' >>. token)

            let private acceptExtensions =
                prefix (skipChar ';') acceptExtension 
                |>> Map.ofList

            let private acceptParameters =
                weight .>> OWS .>>. acceptExtensions

            let private parameter =
                notFollowedBy (OWS >>. skipStringCI "q=") >>. token .>> skipChar '=' .>>. token

            let private parameters =
                prefix (skipChar ';') parameter 
                |>> Map.ofList

            let private openRange = 
                skipString "*/*"
                |>> fun _ -> MediaRange.Partial (Open)

            let private partialRange = 
                token .>> skipString "/*"
                |>> fun x -> MediaRange.Partial (Partial (MediaType x))

            let private fullRange = 
                token .>> skipChar '/' .>>. token
                |>> fun (x, y) -> MediaRange.Specified (Closed (MediaType x, MediaSubType y))

            let private range = 
                choice [
                    attempt openRange
                    attempt partialRange
                    fullRange ]

            let private mediaRange = 
                range .>> OWS .>>. parameters

            let accept = 
                infix (skipChar ',') (mediaRange .>> OWS .>>. opt acceptParameters)
                |>> List.map (fun ((mediaRange, parameters), acceptParameters) ->
                    { MediaRange = mediaRange
                      MediaRangeParameters = parameters
                      Weight = acceptParameters |> Option.map fst
                      ExtensionParameters = acceptParameters |> Option.map snd |> Option.getOrElse Map.empty })


        [<AutoOpen>]
        module Section_5_3_3 =

            (* Accept-Charset

               Taken from RFC 7231, Section 5.3.3. Accept-Charset
               [http://tools.ietf.org/html/rfc7231#section-5.3.3] *)

            let private openCharset =
                skipChar '*'
                |>> fun _ -> Charset.Any

            let private namedCharset =
                token
                |>> fun s -> Charset.Specified (SpecifiedCharset.Named s)

            let private charset = 
                choice [
                    openCharset
                    namedCharset ]

            let acceptCharset =
                infix1 (skipChar ',') (charset .>> OWS .>>. opt weight)
                |>> List.map (fun (charset, weight) ->
                    { Charset = charset
                      Weight = weight })


        [<AutoOpen>]
        module Section_5_3_4 =

            (* Accept-Encoding

               Taken from RFC 7231, Section 5.3.4. Accept-Encoding
               [http://tools.ietf.org/html/rfc7231#section-5.3.4] *)

            let private openEncoding =
                skipChar '*'
                |>> fun _ -> Encoding.Any

            let private identityEncoding =
                skipStringCI "identity"
                |>> fun _ -> Encoding.Specified (SpecifiedEncoding.Identity)

            let private namedEncoding =
                token
                |>> fun s -> Encoding.Specified (SpecifiedEncoding.Named s)

            let private encoding =
                choice [
                    openEncoding
                    identityEncoding
                    namedEncoding ]

            let acceptEncoding =
                infix (skipChar ',') (encoding .>> OWS .>>. opt weight)
                |>> List.map (fun (encoding, weight) ->
                    { Encoding = encoding
                      Weight = weight })

        [<AutoOpen>]
        module Section_5_3_5 =

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

            let acceptLanguage =
                infix (skipChar ',') (languageRange .>> OWS .>>. opt weight)
                |>> List.map (fun (languageRange, weight) ->
                    { Language = languageRange
                      Weight = weight })


    [<AutoOpen>]
    module RFC_7232 =

        (* TODO: This is a naive formulation of an entity tag and does not
           properly support the grammar, particularly weak references, which
           should be implemented ASAP *)

        let entityTag =
            skipChar DQUOTE >>. token .>> skipChar DQUOTE


        [<AutoOpen>]
        module Section_3_1 =

            (* If-Match

               Taken from RFC 7232, Section 3.1, If-Match
               [http://tools.ietf.org/html/rfc7232#section-3.1] *)

            let ifMatch =
                choice [
                    skipChar '*' |>> fun _ -> IfMatch.Any
                    infix (skipChar ',') entityTag |>> fun x -> 
                        IfMatch.EntityTags (List.map Strong x) ]


        [<AutoOpen>]
        module Section_3_2 =

            (* If-None-Match

               Taken from RFC 7232, Section 3.2, If-None-Match
               [http://tools.ietf.org/html/rfc7232#section-3.2] *)

            let ifNoneMatch =
                choice [
                    skipChar '*' |>> fun _ -> IfNoneMatch.Any
                    infix (skipChar ',') entityTag |>> fun x -> 
                        IfNoneMatch.EntityTags (List.map Strong x) ]


[<AutoOpen>]
module Isomorphisms =

    let private weightToString =
        Option.map (sprintf ";q=%.4g") >> Option.getOrElse ""

    // Accept

    let private acceptFromString =
        parse accept

    let private acceptToString =
        List.map (fun (x) ->
            let mediaRange =
                match x.MediaRange with
                | MediaRange.Specified (Closed (MediaType x, MediaSubType y)) -> sprintf "%s/%s" x y
                | MediaRange.Partial (Partial (MediaType x)) -> sprintf "%s/*" x
                | MediaRange.Partial (Open) -> "*/*"

            let mediaRangeParameters =
                match x.MediaRangeParameters.Count with
                | 0 -> ""
                | _ ->
                    x.MediaRangeParameters
                    |> Map.toArray
                    |> Array.map (fun (x, y) -> sprintf "%s=%s" x y)
                    |> Array.rev
                    |> String.concat ";"

            sprintf "%s%s%s" mediaRange mediaRangeParameters (weightToString x.Weight))
        >> String.concat ","

    let internal acceptPIso =
        acceptFromString, acceptToString

    // AcceptCharset

    let private acceptCharsetFromString =
        parse acceptCharset

    let private acceptCharsetToString =
        List.map (fun x ->
            let charset =
                match x.Charset with
                | Charset.Specified (SpecifiedCharset.Named x) -> x
                | Charset.Any -> "*"                    

            sprintf "%s%s" charset (weightToString x.Weight))
        >> String.concat ","

    let internal acceptCharsetPIso =
        acceptCharsetFromString, acceptCharsetToString

    // AcceptEncoding

    let private acceptEncodingFromString =
        parse acceptEncoding

    let private acceptEncodingToString =
        List.map (fun x ->
            let encoding =
                match x.Encoding with
                | Encoding.Specified (SpecifiedEncoding.Named x) -> x
                | Encoding.Specified (SpecifiedEncoding.Identity) -> "identity"
                | Encoding.Any -> "*"                    

            sprintf "%s%s" encoding (weightToString x.Weight)) 
        >> String.concat ","

    let internal acceptEncodingPIso =
        acceptEncodingFromString, acceptEncodingToString

    // AcceptLanguage

    let private acceptLanguageFromString =
        parse acceptLanguage

    let private acceptLanguageToString =
        List.map (fun x -> sprintf "%s%s" x.Language.Name (weightToString x.Weight)) 
        >> String.concat ","

    let internal acceptLanguagePIso =
        acceptLanguageFromString, acceptLanguageToString

    // Box

    let boxIso<'T> : Iso<obj,'T> =
        unbox<'T>, box

    // DateTime

    let private dateTimeFromString x =
        let format = CultureInfo.InvariantCulture.DateTimeFormat
        let adjustment = DateTimeStyles.AdjustToUniversal

        match DateTime.TryParse (x, format, adjustment) with
        | true, x -> Some x
        | _ -> None

    let private dateTimeToString (x: DateTime) =
        x.ToString("r")

    let internal dateTimePIso =
        dateTimeFromString, dateTimeToString

    // ETag

    let private eTagFromString =
        Strong >> Some

    let private eTagToString =
        function | Strong x -> sprintf "\"%s\"" x
                    | Weak x -> sprintf "W/\"%s\"" x
        
    let internal eTagPIso =
        eTagFromString, eTagToString

    // Header

    let headerIso =
        (fun s -> String.concat "," s),
        (fun s -> [| s |])

    // IfMatch

    let private ifMatchFromString =
        parse ifMatch

    let private ifMatchToString =
        function | IfMatch.EntityTags x ->  List.map (snd eTagPIso) x |> String.concat ","
                 | IfMatch.Any -> "*"

    let internal ifMatchPIso =
        ifMatchFromString, ifMatchToString

    // IfNoneMatch

    let private ifNoneMatchFromString =
        parse ifNoneMatch

    let private ifNoneMatchToString =
        function | IfNoneMatch.EntityTags x -> List.map (snd eTagPIso) x |> String.concat ","
                 | IfNoneMatch.Any -> "*"

    let internal ifNoneMatchPIso =
        ifNoneMatchFromString, ifNoneMatchToString

    // Integer

    let private intFromString x =
        match Int32.TryParse x with
        | true, x -> Some x
        | _ -> None

    let internal intPIso =
        intFromString, string

    // Method

    let private methodFromString =
        function | "DELETE" -> DELETE 
                 | "HEAD" -> HEAD 
                 | "GET" -> GET 
                 | "OPTIONS" -> OPTIONS
                 | "PATCH" -> PATCH 
                 | "POST" -> POST 
                 | "PUT" -> PUT 
                 | "TRACE" -> TRACE
                 | x -> Method.Custom x        
                     
    let private methodToString =
        function | DELETE -> "DELETE" 
                 | HEAD -> "HEAD" 
                 | GET -> "GET" 
                 | OPTIONS -> "OPTIONS"
                 | PATCH -> "PATCH" 
                 | POST -> "POST" 
                 | PUT -> "PUT"  
                 | TRACE -> "TRACE"
                 | Method.Custom x -> x

    let internal methodIso =
        methodFromString, methodToString

    // Protocol

    let private protocolFromString =
        function | "HTTP/1.0" -> Protocol.HTTP 1.0 
                 | "HTTP/1.1" -> Protocol.HTTP 1.1 
                 | x -> Protocol.Custom x

    let private protocolToString =
        function | Protocol.HTTP x -> sprintf "HTTP/%.2g" x 
                 | Protocol.Custom x -> x

    let internal protocolIso =
        protocolFromString, protocolToString

    // Scheme

    let private schemeFromString =
        function | "http" -> HTTP 
                 | "https" -> HTTPS 
                 | x -> Scheme.Custom x

    let private schemeToString =    
        function | HTTP -> "http"
                 | HTTPS -> "https" 
                 | Scheme.Custom x -> x

    let internal schemeIso =
        schemeFromString, schemeToString

    // Query

    let private queryFromString =
        function | "" -> Map.empty
                 | s ->
                     s.Split [| '&' |]
                     |> Array.map (fun x -> x.Split [| '=' |])
                     |> Array.map (fun x -> x.[0], x.[1])
                     |> Map.ofArray

    let private queryToString m =
        Map.toArray m
        |> Array.map (fun (x, y) -> sprintf "%s=%s" x y)
        |> Array.rev
        |> String.concat "&"

    let internal queryIso =
        queryFromString, queryToString


[<AutoOpen>]
module Lenses =

    let dictLens k : Lens<IDictionary<'k,'v>, 'v> =
        (fun d -> d.[k]),
        (fun v d -> d.[k] <- v; d)

    let dictPLens k : PLens<IDictionary<'k,'v>, 'v> =
        (fun d -> d.TryGetValue k |> function | true, v -> Some v | _ -> None),
        (fun v d -> d.[k] <- v; d)   


[<RequireQualifiedAccess>]
module Request =

    let body =
             dictLens Constants.requestBody
        <--> boxIso<Stream>

    let headers =
             dictLens Constants.requestHeaders
        <--> boxIso<IDictionary<string, string []>>

    let headersKey key =
             headers
        >-?> dictPLens key

    let meth = 
             dictLens Constants.requestMethod
        <--> boxIso<string>
        <--> methodIso

    let path = 
             dictLens Constants.requestPath
        <--> boxIso<string>

    let pathBase =
             dictLens Constants.requestPathBase
        <--> boxIso<string>

    let protocol =
             dictLens Constants.requestProtocol
        <--> boxIso<string>
        <--> protocolIso

    let scheme = 
             dictLens Constants.requestScheme
        <--> boxIso<string>
        <--> schemeIso

    let query =
             dictLens Constants.requestQueryString
        <--> boxIso<string>
        <--> queryIso

    let queryKey key =
             query
        >-?> mapPLens key


    [<RequireQualifiedAccess>]
    module Headers =

        let accept =
                 headersKey "Accept"
            <?-> headerIso
            <??> acceptPIso

        let acceptCharset =
                 headersKey "Accept-Charset"
            <?-> headerIso
            <??> acceptCharsetPIso

        let acceptEncoding =
                 headersKey "Accept-Encoding"
            <?-> headerIso
            <??> acceptEncodingPIso

        let acceptLanguage =
                 headersKey "Accept-Language"
            <?-> headerIso
            <??> acceptLanguagePIso

        // TODO: typed Authorization

        let authorization =
                 headersKey "Authorization"
            <?-> headerIso

        // TODO: typed CacheControl

        let cacheControl =
                 headersKey "Cache-Control"
            <?-> headerIso

        // TODO: typed Connection

        let connection =
                 headersKey "Connection"
            <?-> headerIso

        // TODO: typed ContentEncoding

        let contentEncoding =
                 headersKey "Content-Encoding"
            <?-> headerIso

        // TODO: typed ContentLanguage

        let contentLanguage =
                 headersKey "Content-Language"
            <?-> headerIso

        let contentLength =
                 headersKey "Content-Length"
            <?-> headerIso
            <??> intPIso

        // TODO: typed ContentLocation

        let contentLocation =
                 headersKey "Content-Location"
            <?-> headerIso

        // TODO: typed ContentMD5

        let contentMD5 =
                 headersKey "Content-MD5"
            <?-> headerIso

        // TODO: typed ContentType

        let contentType =
                 headersKey "Content-Type"
            <?-> headerIso

        let date =
                 headersKey "Date"
            <?-> headerIso
            <??> dateTimePIso

        // TODO: typed Expect

        let expect =
                 headersKey "Expect"
            <?-> headerIso

        // TODO: typed From

        let from =
                 headersKey "From"
            <?-> headerIso

        // TODO: typed Host

        let host =
                 headersKey "Host"
            <?-> headerIso

        let ifMatch =
                 headersKey "If-Match"
            <?-> headerIso
            <??> ifMatchPIso

        let ifModifiedSince =
                 headersKey "If-Modified-Since"
            <?-> headerIso
            <??> dateTimePIso

        let ifNoneMatch =
                 headersKey "If-None-Match"
            <?-> headerIso
            <??> ifNoneMatchPIso

        // TODO: typed IfRange

        let ifRange =
                 headersKey "If-Range"
            <?-> headerIso

        let ifUnmodifiedSince =
                 headersKey "If-Unmodified-Since"
            <?-> headerIso
            <??> dateTimePIso

        let maxForwards =
                 headersKey "Max-Forwards"
            <?-> headerIso
            <??> intPIso

        // TODO: typed Pragma

        let pragma =
                 headersKey "Pragma"
            <?-> headerIso

        // TODO: typed ProxyAuthorization

        let proxyAuthorization =
                 headersKey "Proxy-Authorization"
            <?-> headerIso

        // TODO: typed Range

        let range =
                 headersKey "Range"
            <?-> headerIso

        // TODO: typed Referer

        let referer =
                 headersKey "Referer"
            <?-> headerIso

        // TODO: typed TE

        let TE =
                 headersKey "TE"
            <?-> headerIso

        // TODO: typed Trailer

        let trailer =
                 headersKey "Trailer"
            <?-> headerIso

        // TODO: typed TransferEncoding

        let transferEncoding =
                 headersKey "Transfer-Encoding"
            <?-> headerIso

        // TODO: typed Upgrade

        let upgrade =
                 headersKey "Upgrade"
            <?-> headerIso

        // TODO: typed UserAgent

        let userAgent =
                 headersKey "User-Agent"
            <?-> headerIso

        // TODO: typed Via

        let via =
                 headersKey "Via"
            <?-> headerIso


[<RequireQualifiedAccess>]
module Response =

    let body =
             dictLens Constants.responseBody
        <--> boxIso<Stream>

    let headers =
             dictLens Constants.responseHeaders
        <--> boxIso<IDictionary<string, string []>>

    let headersKey key =
             headers
        >-?> dictPLens key

    let protocol =
             dictPLens Constants.responseProtocol
        <?-> boxIso<string>
        <?-> protocolIso

    let reasonPhrase =
             dictPLens Constants.responseReasonPhrase
        <?-> boxIso<string>

    let statusCode =
             dictPLens Constants.responseStatusCode
        <?-> boxIso<int>


    [<RequireQualifiedAccess>]
    module Headers =

        // TODO: typed AcceptRanges

        let acceptRanges =
                 headersKey "Accept-Ranges"
            <?-> headerIso

        let age =
                 headersKey "Age"
            <?-> headerIso
            <??> intPIso

        // TODO: typed Allow

        let allow =
                 headersKey "Allo"
            <?-> headerIso

        // TODO: typed CacheControl

        let cacheControl =
                 headersKey "Cache-Control"
            <?-> headerIso

        // TODO: typed Connection

        let connection =
                 headersKey "Connection"
            <?-> headerIso

        // TODO: typed ContentEncoding

        let contentEncoding =
                 headersKey "Content-Encoding"
            <?-> headerIso

        // TODO: typed ContentLanguage

        let contentLanguage =
                 headersKey "Content-Language"
            <?-> headerIso

        let contentLength =
                 headersKey "Content-Length"
            <?-> headerIso
            <??> intPIso

        // TODO: typed ContentLocation

        let contentLocation =
                 headersKey "Content-Location"
            <?-> headerIso

        // TODO: typed ContentMD5

        let contentMD5 =
                 headersKey "Content-MD5"
            <?-> headerIso

        // TODO: typed ContentRange

        let contentRange =
                 headersKey "Content-Range"
            <?-> headerIso

        // TODO: typed ContentType

        let contentType =
                 headersKey "Content-Type"
            <?-> headerIso

        let date =
                 headersKey "Date"
            <?-> headerIso
            <??> dateTimePIso

        let eTag =
                 headersKey "ETag"
            <?-> headerIso
            <??> eTagPIso

        let expires =
                 headersKey "Expires"
            <?-> headerIso
            <??> dateTimePIso

        let lastModified =
                 headersKey "Last-Modified"
            <?-> headerIso
            <??> eTagPIso

        // TODO: typed Location

        let location =
                 headersKey "Location"
            <?-> headerIso

        // TODO: typed ProxyAuthenticate

        let proxyAuthenticate =
                 headersKey "Proxy-Authenticate"
            <?-> headerIso

        // TODO: typed RetryAfter

        let retryAfter =
                 headersKey "Retry-After"
            <?-> headerIso

        // TODO: typed Server

        let server =
                 headersKey "Server"
            <?-> headerIso

        // TODO: typed Trailer

        let trailer =
                 headersKey "Trailer"
            <?-> headerIso

        // TODO: typed TransferEncoding

        let transferEncoding =
                 headersKey "Transfer-Encoding"
            <?-> headerIso

        // TODO: typed Upgrade

        let upgrade =
                 headersKey "Upgrade"
            <?-> headerIso

        // TODO: typed Vary

        let vary =
                 headersKey "Vary"
            <?-> headerIso

        // TODO: typed Warning

        let warning =
                 headersKey "Warning"
            <?-> headerIso

        // TODO: typed WWWAuthenticate

        let wwwAuthenticate =
                 headersKey "WWW-Authenticate"
            <?-> headerIso


[<AutoOpen>]
module Monad =

    /// Gets part of the OwinEnv using an Aether lens within an OWIN monad
    let getLM l = 
        getL l <!> getM

    /// Sets part of the OwinEnv using an Aether lens within an OWIN monad
    let setLM l v = 
        setL l v |> modM

    /// Modifies part of the OwinEnv using an Aether lens within an OWIN monad
    let modLM l f = 
        modL l f |> modM

    /// Gets part of the OwinEnv using a partial Aether lens within an OWIN monad
    let getPLM l = 
        getPL l <!> getM

    /// Sets part of the OwinEnv using a partial Aether lens within an OWIN monad
    let setPLM l v = 
        setPL l v |> modM

    /// Modifies part of the OwinEnv using a partial Aether lens within an OWIN monad
    let modPLM l f = 
        modL l f |> modM
