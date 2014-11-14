[<AutoOpen>]
module Freya.Http.Lenses

open System.Collections.Generic
open System.IO
open Aether
open Aether.Operators
open Freya.Core
open Freya.Core.Operators

(* Lenses *)

let dictLens k : Lens<IDictionary<'k,'v>, 'v> =
    (fun d -> d.[k]),
    (fun v d -> d.[k] <- v; d)

let dictPLens k : PLens<IDictionary<'k,'v>, 'v> =
    (fun d -> d.TryGetValue k |> function | true, v -> Some v | _ -> None),
    (fun v d -> d.[k] <- v; d)

let private itemLens<'a> key =
    dictLens key <--> Isomorphisms.Generic.boxIso<'a>

let private itemPLens<'a> key =
    dictPLens key <?-> Isomorphisms.Generic.boxIso<'a>


[<RequireQualifiedAccess>]
module Request =

    let body =
        itemLens<Stream> Constants.requestBody

    let headers =
        itemLens<IDictionary<string, string []>> Constants.requestHeaders

    let headersKey key =
        headers >-?> dictPLens key <?-> Isomorphisms.Generic.headerIso

    let meth = 
        itemLens<string> Constants.requestMethod <--> Isomorphisms.RFC7230.methodIso

    let path = 
        itemLens<string> Constants.requestPath

    let pathBase =
        itemLens<string> Constants.requestPathBase

    let httpVersion =
        itemLens<string> Constants.requestProtocol <--> Isomorphisms.RFC7230.httpVersionIso

    let scheme =
        itemLens<string> Constants.requestScheme <--> Isomorphisms.Generic.schemeIso

    let query =
        itemLens<string> Constants.requestQueryString // <--> TODO: Isomorphism

// TODO: Reinstate when query is iso again

//    let queryKey key =
//        query >-?> mapPLens key

    [<RequireQualifiedAccess>]
    module Headers =

        let accept =
            headersKey "Accept" <??> Isomorphisms.RFC7231.acceptPIso

        let acceptCharset =
            headersKey "Accept-Charset" <??> Isomorphisms.RFC7231.acceptCharsetPIso

        let acceptEncoding =
            headersKey "Accept-Encoding" <??> Isomorphisms.RFC7231.acceptEncodingPIso

        let acceptLanguage =
            headersKey "Accept-Language" <??> Isomorphisms.RFC7231.acceptLanguagePIso

        // TODO: typed Authorization

        let authorization =
            headersKey "Authorization"

        // TODO: typed CacheControl

        let cacheControl =
            headersKey "Cache-Control"

        let connection =
            headersKey "Connection" <??> Isomorphisms.RFC7230.connectionPIso

        let contentEncoding =
            headersKey "Content-Encoding" <??> Isomorphisms.RFC7231.contentEncodingPIso

        // TODO: typed ContentLanguage

        let contentLanguage =
            headersKey "Content-Language"

        let contentLength =
            headersKey "Content-Length" <??> Isomorphisms.RFC7230.contentLengthPIso

        // TODO: typed ContentLocation

        let contentLocation =
            headersKey "Content-Location"

        // TODO: typed ContentMD5

        let contentMD5 =
            headersKey "Content-MD5"

        let contentType =
            headersKey "Content-Type" <??> Isomorphisms.RFC7231.contentTypePIso

        let date =
            headersKey "Date" <??> Isomorphisms.RFC7231.datePIso

        // TODO: typed Expect

        let expect =
            headersKey "Expect"

        // TODO: typed From

        let from =
            headersKey "From"

        // TODO: typed Host

        let host =
            headersKey "Host"

        let ifMatch =
            headersKey "If-Match" <??> Isomorphisms.RFC7232.ifMatchPIso

        let ifModifiedSince =
            headersKey "If-Modified-Since" <??> Isomorphisms.RFC7232.ifModifiedSincePIso

        let ifNoneMatch =
            headersKey "If-None-Match" <??> Isomorphisms.RFC7232.ifNoneMatchPIso

        // TODO: typed IfRange

        let ifRange =
            headersKey "If-Range"

        let ifUnmodifiedSince =
            headersKey "If-Unmodified-Since" <??> Isomorphisms.RFC7232.ifUnmodifiedSincePIso

        let maxForwards =
            headersKey "Max-Forwards" <??> Isomorphisms.RFC7231.maxForwardsPIso

        // TODO: typed Pragma

        let pragma =
            headersKey "Pragma"

        // TODO: typed ProxyAuthorization

        let proxyAuthorization =
            headersKey "Proxy-Authorization"

        // TODO: typed Range

        let range =
            headersKey "Range"

        // TODO: typed Referer

        let referer =
            headersKey "Referer"

        // TODO: typed TE

        let TE =
            headersKey "TE"

        // TODO: typed Trailer

        let trailer =
            headersKey "Trailer"

        // TODO: typed TransferEncoding

        let transferEncoding =
            headersKey "Transfer-Encoding"

        // TODO: typed Upgrade

        let upgrade =
            headersKey "Upgrade"

        // TODO: typed UserAgent

        let userAgent =
            headersKey "User-Agent"

        // TODO: typed Via

        let via =
            headersKey "Via"

[<RequireQualifiedAccess>]
module Response =

    let body =
        itemLens<Stream> Constants.responseBody

    let headers =
        itemLens<IDictionary<string, string []>> Constants.responseHeaders

    let headersKey key =
        headers >-?> dictPLens key <?-> Isomorphisms.Generic.headerIso

    let httpVersion =
        itemPLens<string> Constants.responseProtocol <?-> Isomorphisms.RFC7230.httpVersionIso

    let reasonPhrase =
        itemPLens<string> Constants.responseReasonPhrase

    let statusCode =
        itemPLens<int> Constants.responseStatusCode


    [<RequireQualifiedAccess>]
    module Headers =

        // TODO: typed AcceptRanges

        let acceptRanges =
            headersKey "Accept-Ranges"

        let age =
            headersKey "Age" <??> Isomorphisms.RFC7234.agePIso

        let allow =
            headersKey "Allow" <??> Isomorphisms.RFC7231.allowPIso

        // TODO: typed CacheControl

        let cacheControl =
            headersKey "Cache-Control"

        let connection =
            headersKey "Connection" <??> Isomorphisms.RFC7230.connectionPIso

        let contentEncoding =
            headersKey "Content-Encoding" <??> Isomorphisms.RFC7231.contentEncodingPIso

        // TODO: typed ContentLanguage

        let contentLanguage =
            headersKey "Content-Language"

        let contentLength =
            headersKey "Content-Length" <??> Isomorphisms.RFC7230.contentLengthPIso

        // TODO: typed ContentLocation

        let contentLocation =
            headersKey "Content-Location"

        // TODO: typed ContentMD5

        let contentMD5 =
            headersKey "Content-MD5"

        // TODO: typed ContentRange

        let contentRange =
            headersKey "Content-Range"

        let contentType =
            headersKey "Content-Type" <??> Isomorphisms.RFC7231.contentTypePIso

        let date =
            headersKey "Date" <??> Isomorphisms.RFC7231.datePIso

        let eTag =
            headersKey "ETag" <??> Isomorphisms.RFC7232.eTagPIso

        let expires =
            headersKey "Expires" <??> Isomorphisms.RFC7234.expiresPIso

        let lastModified =
            headersKey "Last-Modified" <??> Isomorphisms.RFC7232.lastModifiedPIso

        // TODO: typed Location

        let location =
            headersKey "Location"

        // TODO: typed ProxyAuthenticate

        let proxyAuthenticate =
            headersKey "Proxy-Authenticate"

        // TODO: typed RetryAfter

        let retryAfter =
            headersKey "Retry-After"

        // TODO: typed Server

        let server =
            headersKey "Server"

        // TODO: typed Trailer

        let trailer =
            headersKey "Trailer"

        // TODO: typed TransferEncoding

        let transferEncoding =
            headersKey "Transfer-Encoding"

        // TODO: typed Upgrade

        let upgrade =
            headersKey "Upgrade"

        // TODO: typed Vary

        let vary =
            headersKey "Vary"

        // TODO: typed Warning

        let warning =
            headersKey "Warning"

        // TODO: typed WWWAuthenticate

        let wwwAuthenticate =
            headersKey "WWW-Authenticate"


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
        modPL l f |> modM
