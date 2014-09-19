﻿namespace Dyfrig.Http

open System.Collections.Generic
open System.IO
open Aether
open Aether.Operators
open Dyfrig.Core
open Dyfrig.Core.Operators
open FParsec


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


[<AutoOpen>]
module Headers =

    [<AutoOpen>]
    module Request =

        // Content Negotiation

        (* Accept
            Taken from RFC 7231, Section 5.3.2. Accept
            [http://tools.ietf.org/html/rfc7231#section-5.3.2] *)

        type Accept =
            { MediaRange: MediaRange
              AcceptParameters: AcceptParameters option }

        and MediaRange =
            { Type: MediaType
              SubType: MediaType
              Parameters: Map<string, string> }

        and MediaType =
            | Named of string
            | Any

        and AcceptParameters =
            { Weight: float
              AcceptExtensions: Map<string, string option> }

        (* Accept-Charset
            Taken from RFC 7231, Section 5.3.3. Accept-Charset
            [http://tools.ietf.org/html/rfc7231#section-5.3.3] *)

        type AcceptCharset =
            { Charset: Charset
              Weight: float option }

        and Charset =
            | Named of string
            | Any


[<AutoOpen>]
module internal Parsers =

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
                |>> fun (weight, acceptExtensions) -> 
                    { Weight = weight
                      AcceptExtensions = acceptExtensions }

            let private parameter =
                notFollowedBy (OWS >>. skipStringCI "q=") >>. token .>> skipChar '=' .>>. token

            let private parameters =
                prefix (skipChar ';') parameter 
                |>> Map.ofList

            let private openRange = 
                skipString "*/*"
                |>> fun _ -> MediaType.Any, MediaType.Any

            let private partialRange = 
                token .>> skipString "/*"
                |>> fun x -> MediaType.Named x, MediaType.Any

            let private fullRange = 
                token .>> skipChar '/' .>>. token
                |>> fun (x, y) -> MediaType.Named x, MediaType.Named y

            let private range = 
                choice [
                    attempt openRange
                    attempt partialRange
                    fullRange ]

            let private mediaRange = 
                range .>> OWS .>>. parameters
                |>> fun ((t, st), parameters) ->
                    { Type = t
                      SubType = st
                      Parameters = parameters } 

            let accept = 
                infix (skipChar ',') (mediaRange .>> OWS .>>. opt acceptParameters)
                |>> List.map (fun (mediaRange, acceptParameters) ->
                    { MediaRange = mediaRange
                      AcceptParameters = acceptParameters })


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
                |>> fun s -> Charset.Named s

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


[<AutoOpen>]
module Lenses =

    // Dictionary

    let dictLens k : Lens<IDictionary<'k,'v>, 'v> =
        ((fun d -> d.[k]),
         (fun v d -> d.[k] <- v; d))

    let dictPLens k : PLens<IDictionary<'k,'v>, 'v> =
        ((fun d -> d.TryGetValue k |> function | true, v -> Some v | _ -> None),
         (fun v d -> d.[k] <- v; d))

    let dictOptionLens k : Lens<IDictionary<'k,'v>, 'v option> =
        ((fun d -> d.TryGetValue k |> function | true, v -> Some v | _ -> None),
         (fun v d -> v |> function | Some v -> d.[k] <- v; d | _ -> d.Remove k |> ignore; d))

    // Request

    let internal isoMethodLens : Lens<string, Method> =
        (fun s -> methodFromString s), 
        (fun m _ -> methodToString m)

    let internal isoProtocolLens : Lens<string, Protocol> =
        (fun s -> protocolFromString s), 
        (fun p _ -> protocolToString p)

    let internal isoSchemeLens : Lens<string, Scheme> =
        (fun s -> schemeFromString s), 
        (fun s _ -> schemeToString s)

    let internal isoQueryLens : Lens<string, Map<string, string>> =
        (fun q -> queryFromString q),
        (fun m _ -> queryToString m)

    // Content Negotiation

    let internal isoAcceptPLens : PLens<string [] option, Accept list> =
        ((fun s -> Option.bind (fun s -> parse accept (String.concat "," s)) s), 
         (fun _ _ -> Some (Array.ofList [ "test" ])))

    let internal isoAcceptCharsetPLens : PLens<string [] option, AcceptCharset list> =
        ((fun s -> Option.bind (fun s -> parse acceptCharset (String.concat "," s)) s),
         (fun _ _ -> Some (Array.ofList [ "test" ])))


[<RequireQualifiedAccess>]
module Request =

    let body =
        dictLens Constants.requestBody
        >--> isoLens unbox<Stream> box

    let header key =
        dictLens Constants.requestHeaders
        >--> isoLens unbox<IDictionary<string, string []>> box
        >--> dictOptionLens key

    let meth =
        dictLens Constants.requestMethod
        >--> isoLens unbox<string> box
        >--> isoMethodLens

    let path =
        dictLens Constants.requestPath
        >--> isoLens unbox<string> box

    let pathBase =
        dictLens Constants.requestPathBase
        >--> isoLens unbox<string> box

    let protocol =
        dictLens Constants.requestProtocol
        >--> isoLens unbox<string> box
        >--> isoProtocolLens

    let scheme = 
        dictLens Constants.requestScheme
        >--> isoLens unbox<string> box
        >--> isoSchemeLens

    let query key =
        dictLens Constants.requestQueryString
        >--> isoLens unbox<string> box
        >--> isoQueryLens
        >-?> mapPLens key


    [<RequireQualifiedAccess>]
    module Headers =

        let accept =
            dictLens Constants.requestHeaders
            >--> isoLens unbox<IDictionary<string, string []>> box
            >--> dictOptionLens "Accept"
            >-?> isoAcceptPLens

        let acceptCharset =
            dictLens Constants.requestHeaders
            >--> isoLens unbox<IDictionary<string, string []>> box
            >--> dictOptionLens "Accept-Charset"
            >-?> isoAcceptCharsetPLens


[<RequireQualifiedAccess>]
module Response =

    let body =
        dictLens Constants.responseBody
        >--> isoLens unbox<Stream> box

    let header key =
        dictLens Constants.responseHeaders
        >--> isoLens unbox<IDictionary<string, string []>> box
        >--> dictOptionLens key

    let reasonPhrase =
        dictPLens Constants.responseReasonPhrase
        >?-> isoLens unbox<string> box

    let statusCode =
        dictPLens Constants.responseStatusCode
        >?-> isoLens unbox<int> box


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