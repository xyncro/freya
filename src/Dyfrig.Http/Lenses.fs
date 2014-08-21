namespace Dyfrig.Http

open System.Collections.Generic
open System.IO
open Aether
open Aether.Operators
open Dyfrig

[<AutoOpen>]
module Common =

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
