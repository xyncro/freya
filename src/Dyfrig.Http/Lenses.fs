[<AutoOpen>]
module Dyfrig.Http.Lenses

open System.Collections.Generic
open System.IO
open Aether
open Aether.Operators
open Dyfrig.Core
open Dyfrig.Core.Operators

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
        headers >-?> dictPLens key

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

        let private header key =
            headersKey key <?-> Isomorphisms.Generic.headerIso

        let accept =
            header "Accept" <??> Isomorphisms.RFC7231.acceptPIso

        let acceptCharset =
            header "Accept-Charset" <??> Isomorphisms.RFC7231.acceptCharsetPIso

        let acceptEncoding =
            header "Accept-Encoding" <??> Isomorphisms.RFC7231.acceptEncodingPIso

        let acceptLanguage =
            header "Accept-Language" <??> Isomorphisms.RFC7231.acceptLanguagePIso

        // TODO: typed Authorization

        let authorization =
            header "Authorization"

        // TODO: typed CacheControl

        let cacheControl =
            header "Cache-Control"

        let connection =
            header "Connection" <??> Isomorphisms.RFC7230.connectionPIso

        let contentEncoding =
            header "Content-Encoding" <??> Isomorphisms.RFC7231.contentEncodingPIso

        // TODO: typed ContentLanguage

        let contentLanguage =
            header "Content-Language"

        let contentLength =
            header"Content-Length" <??> Isomorphisms.RFC7230.contentLengthPIso

        // TODO: typed ContentLocation

        let contentLocation =
            header "Content-Location"

        // TODO: typed ContentMD5

        let contentMD5 =
            header "Content-MD5"

        let contentType =
            header "Content-Type" <??> Isomorphisms.RFC7231.contentTypePIso

        let date =
            header "Date" <??> Isomorphisms.RFC7231.datePIso

        // TODO: typed Expect

        let expect =
            header "Expect"

        // TODO: typed From

        let from =
            header "From"

        // TODO: typed Host

        let host =
            header "Host"

        let ifMatch =
            header"If-Match" <??> Isomorphisms.RFC7232.ifMatchPIso

        let ifModifiedSince =
            header "If-Modified-Since" <??> Isomorphisms.RFC7232.ifModifiedSincePIso

        let ifNoneMatch =
            header "If-None-Match" <??> Isomorphisms.RFC7232.ifNoneMatchPIso

        // TODO: typed IfRange

        let ifRange =
            header "If-Range"

        let ifUnmodifiedSince =
            header "If-Unmodified-Since" <??> Isomorphisms.RFC7232.ifUnmodifiedSincePIso

        let maxForwards =
            header "Max-Forwards" <??> Isomorphisms.RFC7231.maxForwardsPIso

        // TODO: typed Pragma

        let pragma =
            header "Pragma"

        // TODO: typed ProxyAuthorization

        let proxyAuthorization =
            header "Proxy-Authorization"

        // TODO: typed Range

        let range =
            header "Range"

        // TODO: typed Referer

        let referer =
            header "Referer"

        // TODO: typed TE

        let TE =
            header "TE"

        // TODO: typed Trailer

        let trailer =
            header "Trailer"

        // TODO: typed TransferEncoding

        let transferEncoding =
            header "Transfer-Encoding"

        // TODO: typed Upgrade

        let upgrade =
            header "Upgrade"

        // TODO: typed UserAgent

        let userAgent =
            header "User-Agent"

        // TODO: typed Via

        let via =
            header "Via"

[<RequireQualifiedAccess>]
module Response =

    let body =
        itemLens<Stream> Constants.responseBody

    let headers =
        itemLens<IDictionary<string, string []>> Constants.responseHeaders

    let headersKey key =
        headers >-?> dictPLens key

    let httpVersion =
        itemPLens<string> Constants.responseProtocol <?-> Isomorphisms.RFC7230.httpVersionIso

    let reasonPhrase =
        itemPLens<string> Constants.responseReasonPhrase

    let statusCode =
        itemPLens<int> Constants.responseStatusCode


    [<RequireQualifiedAccess>]
    module Headers =

        let private header key =
            headersKey key <?-> Isomorphisms.Generic.headerIso

        // TODO: typed AcceptRanges

        let acceptRanges =
            header "Accept-Ranges"

        let age =
            header "Age" <??> Isomorphisms.RFC7234.agePIso

        let allow =
            header "Allow" <??> Isomorphisms.RFC7231.allowPIso

        // TODO: typed CacheControl

        let cacheControl =
            header "Cache-Control"

        let connection =
            header "Connection" <??> Isomorphisms.RFC7230.connectionPIso

        let contentEncoding =
            header "Content-Encoding" <??> Isomorphisms.RFC7231.contentEncodingPIso

        // TODO: typed ContentLanguage

        let contentLanguage =
            header "Content-Language"

        let contentLength =
            header "Content-Length" <??> Isomorphisms.RFC7230.contentLengthPIso

        // TODO: typed ContentLocation

        let contentLocation =
            header "Content-Location"

        // TODO: typed ContentMD5

        let contentMD5 =
            header "Content-MD5"

        // TODO: typed ContentRange

        let contentRange =
            header "Content-Range"

        let contentType =
            header "Content-Type" <??> Isomorphisms.RFC7231.contentTypePIso

        let date =
            header "Date" <??> Isomorphisms.RFC7231.datePIso

        let eTag =
            header "ETag" <??> Isomorphisms.RFC7232.eTagPIso

        let expires =
            header "Expires" <??> Isomorphisms.RFC7234.expiresPIso

        let lastModified =
            header "Last-Modified" <??> Isomorphisms.RFC7232.lastModifiedPIso

        // TODO: typed Location

        let location =
            header "Location"

        // TODO: typed ProxyAuthenticate

        let proxyAuthenticate =
            header "Proxy-Authenticate"

        // TODO: typed RetryAfter

        let retryAfter =
            header "Retry-After"

        // TODO: typed Server

        let server =
            header "Server"

        // TODO: typed Trailer

        let trailer =
            header "Trailer"

        // TODO: typed TransferEncoding

        let transferEncoding =
            header "Transfer-Encoding"

        // TODO: typed Upgrade

        let upgrade =
            header "Upgrade"

        // TODO: typed Vary

        let vary =
            header "Vary"

        // TODO: typed Warning

        let warning =
            header "Warning"

        // TODO: typed WWWAuthenticate

        let wwwAuthenticate =
            header "WWW-Authenticate"


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
