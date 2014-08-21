namespace Dyfrig.Http

open System.Collections.Generic
open System.IO
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


[<AutoOpen>]
module internal Helpers =

    // Headers

    let headersFromDict k =
        fun (headers: IDictionary<string, string []>) ->
            match headers.TryGetValue k with
            | true, v -> Some (List.ofArray v)
            | _ -> None

    let headersToDict k =
        fun x (headers: IDictionary<string, string []>) ->
            headers.[k] <- List.toArray x
            headers

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

    let internal isoHeaderPLens k : PLens<IDictionary<string, string []>, string list> =
        (fun headers -> headersFromDict k headers),
        (fun x headers -> headersToDict k x headers)

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

    let owinEnvLens<'T> k : Lens<OwinEnv, 'T> =
        ((fun e -> e.[k]), (fun o e -> e.[k] <- o; e))
        >--> isoLens unbox<'T> box

    let owinEnvPLens<'T> k : PLens<OwinEnv, 'T> =
        ((fun e -> e.TryGetValue k |> function | true, v -> Some v | _ -> None), (fun o e -> e.[k] <- o; e))
        >?-> isoLens unbox<'T> box


[<RequireQualifiedAccess>]
module Request =

    let Body =
        owinEnvLens<Stream> Constants.requestBody

    let Header key =
        owinEnvLens<IDictionary<string, string []>> Constants.requestHeaders
        >-?> isoHeaderPLens key

    let Method =
        owinEnvLens<string> Constants.requestMethod
        >--> isoMethodLens

    let Path =
        owinEnvLens<string> Constants.requestPath

    let Protocol =
        owinEnvLens<string> Constants.requestProtocol
        >--> isoProtocolLens

    let Scheme = 
        owinEnvLens<string> Constants.requestScheme 
        >--> isoSchemeLens

    let Query key =
        owinEnvLens<string> Constants.requestQueryString
        >--> isoQueryLens
        >-?> mapPLens key


[<RequireQualifiedAccess>]
module Response =

    let Body =
        owinEnvLens<Stream> Constants.responseBody

    let Header key =
        owinEnvLens<IDictionary<string, string []>> Constants.responseHeaders
        >-?> isoHeaderPLens key

    let ReasonPhrase =
        owinEnvPLens<string> Constants.responseReasonPhrase

    let StatusCode =
        owinEnvPLens<int> Constants.responseStatusCode


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
