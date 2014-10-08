namespace Dyfrig.Http

open System
open System.Collections.Generic
open System.IO
open System.Globalization
open Aether
open Aether.Operators
open Dyfrig.Core
open Dyfrig.Core.Operators


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

type Protocol =
    | HTTP of float 
    | Custom of string

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
module Headers =

    [<AutoOpen>]
    module Request =

        (* Conditionals
            
           Taken from RFC 7231, Section 5.2 linking to RFC 7232
           [http://tools.ietf.org/html/rfc7231#section-5.2] *)

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

        (* Content Negotiation
            
           Taken from RFC 7231, Section 5.3
           [http://tools.ietf.org/html/rfc7231#section-5.3] *)

        (* Accept

           Taken from RFC 7231, Section 5.3.2. Accept
           [http://tools.ietf.org/html/rfc7231#section-5.3.2] *)

        type Accept =
            { MediaType: MediaRange
              MediaTypeParameters: Map<string, string>
              ExtensionParameters: Map<string, string option>
              Weight: float option }

        and MediaRange =
            | Closed of ClosedMediaRange
            | Partial of PartialMediaRange
            | Open

        and ClosedMediaRange =
            | ClosedMediaRange of MediaType * MediaSubType

        and PartialMediaRange =
            | PartialMediaRange of MediaType

        and MediaType =
            | MediaType of string

        and MediaSubType =
            | MediaSubType of string

        // Negotiation

        // TODO: Make this much better! It's ugly as hell right now...

        let private (|Closed|_|) =
            function | MediaRange.Closed (ClosedMediaRange (MediaType x, MediaSubType y)) -> Some (x, y)
                     | _ -> None

        let private (|Partial|_|) =
            function | MediaRange.Partial (PartialMediaRange (MediaType x)) -> Some x
                     | _ -> None

        let private (===) s1 s2 =
            String.Equals (s1, s2, StringComparison.OrdinalIgnoreCase)

        let private matchAccept (ClosedMediaRange (MediaType t, MediaSubType s)) =
            function | Closed (t', s') when t === t' && s === s' -> true, 3
                     | Partial t' when t === t' -> true, 2
                     | MediaRange.Open -> true, 1
                     | _ -> false, 0

        let negotiateAccept (available: ClosedMediaRange list) (requested: Accept list) =
            requested
            |> List.sortBy (fun r -> r.Weight |> Option.getOrElse 1.)
            |> List.rev
            |> List.tryPick (fun r ->
                let available =
                    available 
                    |> List.map (fun a -> a, matchAccept a r.MediaType)
                    |> List.filter (fun (_, (m, _)) -> m)
                    
                match available with
                | [] -> None
                | available -> Some (r, available |> List.maxBy (fun (_, (_, s)) -> s)))
            |> Option.map (fun (_, (selected, _)) -> selected)

        (* Accept-Charset

           Taken from RFC 7231, Section 5.3.3. Accept-Charset
           [http://tools.ietf.org/html/rfc7231#section-5.3.3] *)

        type AcceptCharset =
            { Charset: Charset
              Weight: float option }

        and Charset =
            | Named of NamedCharset
            | Any

        and NamedCharset =
            | NamedCharset of string

        // Negotiation

        let negotiateCharset (available: NamedCharset list) (requested: AcceptCharset list) =
            None

        (* Accept-Encoding

           Taken from RFC 7231, Section 5.3.4. Accept-Encoding
           [http://tools.ietf.org/html/rfc7231#section-5.3.4] *)

        type AcceptEncoding =
            { Encoding: Encoding
              Weight: float option }

        and Encoding =
            | Named of NamedEncoding
            | Identity
            | Any

        and NamedEncoding =
            | NamedEncoding of string

        // Negotiation

        let negotiateEncoding (available: NamedEncoding list) (requested: AcceptEncoding list) =
            None

        (* Accept-Language

           Taken from RFC 7231, Section 5.3.5. Accept-Language
           [http://tools.ietf.org/html/rfc7231#section-5.3.5] *)

        type AcceptLanguage =
            { Language: CultureInfo
              Weight: float option }

        // Negotiation

        let negotiateLanguage (available: CultureInfo list) (requested: AcceptLanguage list) =
            None


[<AutoOpen>]
module internal Parsers =

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

            let CR = 
                char 0x0d

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

            let VCHAR = 
                set (List.map char [0x21 .. 0x7e])

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

            let prefix1 s p =
                notEmpty (prefix s p)


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
                |>> fun _ -> MediaRange.Open

            let private partialRange = 
                token .>> skipString "/*"
                |>> fun x -> MediaRange.Partial (PartialMediaRange (MediaType x))

            let private fullRange = 
                token .>> skipChar '/' .>>. token
                |>> fun (x, y) -> MediaRange.Closed (ClosedMediaRange (MediaType x, MediaSubType y))

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
                    { MediaType = mediaRange
                      MediaTypeParameters = parameters
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
                |>> fun s -> Charset.Named (NamedCharset s)

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
                |>> fun _ -> Encoding.Identity

            let private namedEncoding =
                token
                |>> fun s -> Encoding.Named (NamedEncoding s)

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
                        IfMatch.EntityTags (List.map EntityTag.Strong x) ]


        [<AutoOpen>]
        module Section_3_2 =

            (* If-None-Match

               Taken from RFC 7232, Section 3.2, If-None-Match
               [http://tools.ietf.org/html/rfc7232#section-3.2] *)

            let ifNoneMatch =
                choice [
                    skipChar '*' |>> fun _ -> IfNoneMatch.Any
                    infix (skipChar ',') entityTag |>> fun x -> 
                        IfNoneMatch.EntityTags (List.map EntityTag.Strong x) ]


[<AutoOpen>]
module internal Helpers =

    // Method

    let methodFromString =
        fun s -> 
            match s with
            | "DELETE" -> DELETE 
            | "HEAD" -> HEAD 
            | "GET" -> GET 
            | "OPTIONS" -> OPTIONS
            | "PATCH" -> PATCH 
            | "POST" -> POST 
            | "PUT" -> PUT 
            | "TRACE" -> TRACE
            | x -> Method.Custom x

    let methodToString =
        fun m -> 
            match m with
            | DELETE -> "DELETE" 
            | HEAD -> "HEAD" 
            | GET -> "GET" 
            | OPTIONS -> "OPTIONS"
            | PATCH -> "PATCH" 
            | POST -> "POST" 
            | PUT -> "PUT"  
            | TRACE -> "TRACE"
            | Method.Custom x -> x

    // Protocol

    let protocolFromString =
        fun s ->
            match s with
            | "HTTP/1.0" -> Protocol.HTTP 1.0 
            | "HTTP/1.1" -> Protocol.HTTP 1.1 
            | x -> Protocol.Custom x
            
    let protocolToString =
        fun p ->
            match p with
            | Protocol.HTTP x -> sprintf "HTTP/%f" x 
            | Protocol.Custom x -> x

    // Scheme
            
    let schemeFromString =
        fun s ->
            match s with
            | "http" -> HTTP 
            | "https" -> HTTPS 
            | x -> Custom x
        
    let schemeToString =    
        fun s ->
            match s with
            | HTTP -> "http" 
            | HTTPS -> "https" 
            | Custom x -> x

    // Query

    (* TODO: This approach to query strings is probably overly naive and should
       be replaced ASAP *)

    let queryFromString =
        fun q ->
            match q with
            | "" -> 
                Map.empty
            | s ->
                s.Split [| '&' |]
                |> Array.map (fun x -> x.Split [| '=' |])
                |> Array.map (fun x -> x.[0], x.[1])
                |> Map.ofArray

    let queryToString =
        fun m ->
            Map.toArray m
            |> Array.map (fun x -> sprintf "%s=%s" (fst x) (snd x))
            |> String.concat "&"

    // DateTime 

    let dateTimeFromString d =
        let format = CultureInfo.InvariantCulture.DateTimeFormat
        let adjustment = DateTimeStyles.AdjustToUniversal

        match DateTime.TryParse (d, format, adjustment) with
        | true, d -> Some d
        | _ -> None

    let dateTimeToString (d: DateTime) =
        d.ToUniversalTime().ToString("r")


[<AutoOpen>]
module Lenses =

    // Boxing

    let boxIso<'T> : Iso<obj,'T> =
        ((unbox<'T>), box)

    // Dictionary

    let dictLens k : Lens<IDictionary<'k,'v>, 'v> =
        (fun d -> d.[k]),
        (fun v d -> d.[k] <- v; d)

    let dictPLens k : PLens<IDictionary<'k,'v>, 'v> =
        (fun d -> d.TryGetValue k |> function | true, v -> Some v | _ -> None),
        (fun v d -> d.[k] <- v; d)

    // Request

    let internal methodIso : Iso<string, Method> =
        (fun s -> methodFromString s), 
        (fun m -> methodToString m)

    let internal protocolIso : Iso<string, Protocol> =
        (fun s -> protocolFromString s), 
        (fun p -> protocolToString p)

    let internal schemeIso : Iso<string, Scheme> =
        (fun s -> schemeFromString s), 
        (fun s -> schemeToString s)

    let internal queryIso : Iso<string, Map<string, string>> =
        (fun q -> queryFromString q),
        (fun m -> queryToString m)

    // Content Negotiation

    let internal acceptPIso : PIso<string [], Accept list> =
        (fun s -> parse accept (String.concat "," s)), 
        (fun _ -> Array.ofList [ "test" ])

    let internal acceptCharsetPIso : PIso<string [], AcceptCharset list> =
        (fun s -> parse acceptCharset (String.concat "," s)),
        (fun _ -> Array.ofList [ "test" ])

    let internal acceptEncodingPIso : PIso<string [], AcceptEncoding list> =
        (fun s -> parse acceptEncoding (String.concat "," s)),
        (fun _ -> Array.ofList [ "test" ])

    let internal acceptLanguagePIso : PIso<string [], AcceptLanguage list> =
        (fun s -> parse acceptLanguage (String.concat "," s)),
        (fun _ -> Array.ofList [ "test" ])

    // Conditionals

    let internal ifMatchPIso : PIso<string [], IfMatch> =
        (fun s -> parse ifMatch (String.concat "," s)),
        (fun _ -> [| "test" |])

    let internal ifNoneMatchPIso : PIso<string [], IfNoneMatch> =
        (fun s -> parse ifNoneMatch (String.concat "," s)),
        (fun _ -> [| "test" |])

    // DateTime

    let dateTimePIso : PIso<string [], DateTime> =
        (fun s -> dateTimeFromString (String.concat "" s)),
        (fun d -> [| dateTimeToString d |])


[<RequireQualifiedAccess>]
module Request =

    let body =
             dictLens Constants.requestBody
        <--> boxIso<Stream>

    let header key =
             dictLens Constants.requestHeaders
        <--> boxIso<IDictionary<string, string []>>
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

    let query key =
             dictLens Constants.requestQueryString
        <--> boxIso<string>
        <--> queryIso
        >-?> mapPLens key


    [<RequireQualifiedAccess>]
    module Headers =

        // Content Negotiation

        let accept =
                 header "Accept"
            <??> acceptPIso

        let acceptCharset =
                 header "Accept-Charset"
            <??> acceptCharsetPIso

        let acceptEncoding =
                 header "Accept-Encoding"
            <??> acceptEncodingPIso

        let acceptLanguage =
                 header "Accept-Language"
            <??> acceptLanguagePIso

        // Conditionals

        let ifMatch =
                 header "If-Match"
            <??> ifMatchPIso

        let ifNoneMatch =
                 header "If-None-Match"
            <??> ifNoneMatchPIso

        let ifModifiedSince =
                 header "If-Modified-Since"
            <??> dateTimePIso

        let ifUnmodifiedSince =
                 header "If-Unmodified-Since"
            <??> dateTimePIso


[<RequireQualifiedAccess>]
module Response =

    let body =
             dictLens Constants.responseBody
        <--> boxIso<Stream>

    let header key =
             dictLens Constants.responseHeaders
        <--> boxIso<IDictionary<string, string []>>
        >-?> dictPLens key

    let reasonPhrase =
             dictPLens Constants.responseReasonPhrase
        <?-> boxIso<string>

    let statusCode =
             dictPLens Constants.responseStatusCode
        <?-> boxIso<int>


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
