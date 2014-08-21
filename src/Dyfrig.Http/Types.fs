namespace Dyfrig.Http

open System.Collections.Generic

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
