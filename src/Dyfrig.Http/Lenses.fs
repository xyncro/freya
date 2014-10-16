[<AutoOpen>]
module Dyfrig.Http.Lenses

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open Aether
open Aether.Operators
open Dyfrig.Core
open Dyfrig.Core.Operators

(* Isomorphisms *)

let boxIso<'T> : Iso<obj,'T> =
    unbox<'T>, box

let private dateTimeFromString x =
    let format = CultureInfo.InvariantCulture.DateTimeFormat
    let adjustment = DateTimeStyles.AdjustToUniversal

    match DateTime.TryParse (x, format, adjustment) with
    | true, x -> Some x
    | _ -> None

let private dateTimeToString (x: DateTime) =
    x.ToString("r")

let private dateTimePIso =
    dateTimeFromString, dateTimeToString

let headerIso =
    (fun s -> String.concat "," s),
    (fun s -> [| s |])

let private intFromString x =
    match Int32.TryParse x with
    | true, x -> Some x
    | _ -> None

let intPIso : PIso<string, int> =
    intFromString, string

(* RFC 7231 *)

let private methodIso =
    parseMethod, formatMethod

let private protocolIso =
    parseProtocol, formatProtocol

let private schemeIso =
    parseScheme, formatScheme

let private queryIso =
    parseQuery, formatQuery

let private acceptPIso =
    parseAccept, formatAccept

let private acceptCharsetPIso =
    parseAcceptCharset, formatAcceptCharset

let private acceptEncodingPIso =
    parseAcceptEncoding, formatAcceptEncoding

let private acceptLanguagePIso =
    parseAcceptLanguage, formatAcceptLanguage

(* RFC 7232 *)
        
let private eTagPIso =
    parseETag, formatETag

let private ifMatchPIso =
    parseIfMatch, formatIfMatch

let private ifNoneMatchPIso =
    parseIfNoneMatch, formatIfNoneMatch

(* Lenses *)

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
