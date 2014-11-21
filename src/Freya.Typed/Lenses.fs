[<AutoOpen>]
module Freya.Typed.Lenses

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
    dictLens key <--> boxIso<'a>

let private itemPLens<'a> key =
    dictPLens key <?-> boxIso<'a>

(* Request Lenses *)

[<RequireQualifiedAccess>]
module Request =

    let body =
        itemLens<Stream> Constants.requestBody

    let headers =
        itemLens<IDictionary<string, string []>> Constants.requestHeaders

    let headersKey key =
        headers >-?> dictPLens key <?-> ((String.concat ","), (Array.create 1))

    let meth = 
        itemLens<string> Constants.requestMethod <--> (Method.Parse, Method.Format)

    let path = 
        itemLens<string> Constants.requestPath

    let pathBase =
        itemLens<string> Constants.requestPathBase

    let httpVersion =
        itemLens<string> Constants.requestProtocol <--> (HttpVersion.Parse, HttpVersion.Format)

    let scheme =
        itemLens<string> Constants.requestScheme

    let query =
        itemLens<string> Constants.requestQueryString // <--> TODO: Isomorphism

// TODO: Reinstate when query is iso again

//    let queryKey key =
//        query >-?> mapPLens key

    (* Request Header Lenses *)

    [<RequireQualifiedAccess>]
    module Headers =

        let accept =
            headersKey "Accept" <??> (Accept.TryParse, Accept.Format)

        let acceptCharset =
            headersKey "Accept-Charset" <??> (AcceptCharset.TryParse, AcceptCharset.Format)

        let acceptEncoding =
            headersKey "Accept-Encoding" <??> (AcceptEncoding.TryParse, AcceptEncoding.Format)

        let acceptLanguage =
            headersKey "Accept-Language" <??> (AcceptLanguage.TryParse, AcceptLanguage.Format)

        // TODO: typed Authorization

        let authorization =
            headersKey "Authorization"

        let cacheControl =
            headersKey "Cache-Control" <??> (CacheControl.TryParse, CacheControl.Format)

        let connection =
            headersKey "Connection" <??> (Connection.TryParse, Connection.Format)

        let contentEncoding =
            headersKey "Content-Encoding" <??> (ContentEncoding.TryParse, ContentEncoding.Format)

        let contentLanguage =
            headersKey "Content-Language" <??> (ContentLanguage.TryParse, ContentLanguage.Format)

        let contentLength =
            headersKey "Content-Length" <??> (ContentLength.TryParse, ContentLength.Format)

        let contentLocation =
            headersKey "Content-Location" <??> (ContentLocation.TryParse, ContentLocation.Format)

        let contentType =
            headersKey "Content-Type" <??> (ContentType.TryParse, ContentType.Format)

        let date =
            headersKey "Date" <??> (Date.TryParse, Date.Format)

        let expect =
            headersKey "Expect" <??> (Expect.TryParse, Expect.Format)

        // TODO: typed From

        let from =
            headersKey "From"

        let host =
            headersKey "Host" <??> (Host.TryParse, Host.Format)

        let ifMatch =
            headersKey "If-Match" <??> (IfMatch.TryParse, IfMatch.Format)

        let ifModifiedSince =
            headersKey "If-Modified-Since" <??> (IfModifiedSince.TryParse, IfModifiedSince.Format)

        let ifNoneMatch =
            headersKey "If-None-Match" <??> (IfNoneMatch.TryParse, IfNoneMatch.Format)

        let ifRange =
            headersKey "If-Range" <??> (IfRange.TryParse, IfRange.Format)

        let ifUnmodifiedSince =
            headersKey "If-Unmodified-Since" <??> (IfUnmodifiedSince.TryParse, IfUnmodifiedSince.Format)

        let maxForwards =
            headersKey "Max-Forwards" <??> (MaxForwards.TryParse, MaxForwards.Format)

        // TODO: typed Pragma

        let pragma =
            headersKey "Pragma"

        // TODO: typed ProxyAuthorization

        let proxyAuthorization =
            headersKey "Proxy-Authorization"

        // TODO: typed Range

        let range =
            headersKey "Range"

        let referer =
            headersKey "Referer" <??> (Referer.TryParse, Referer.Format)

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

(* Response Lenses *)

[<RequireQualifiedAccess>]
module Response =

    let body =
        itemLens<Stream> Constants.responseBody

    let headers =
        itemLens<IDictionary<string, string []>> Constants.responseHeaders

    let headersKey key =
        headers >-?> dictPLens key <?-> ((String.concat ","), (Array.create 1))

    let httpVersion =
        itemPLens<string> Constants.responseProtocol <?-> (HttpVersion.Parse, HttpVersion.Format)

    let reasonPhrase =
        itemPLens<string> Constants.responseReasonPhrase

    let statusCode =
        itemPLens<int> Constants.responseStatusCode

    (* Response Header Lenses *)

    [<RequireQualifiedAccess>]
    module Headers =

        // TODO: typed AcceptRanges

        let acceptRanges =
            headersKey "Accept-Ranges"

        let age =
            headersKey "Age" <??> (Age.TryParse, Age.Format)

        let allow =
            headersKey "Allow" <??> (Allow.TryParse, Allow.Format)

        let cacheControl =
            headersKey "Cache-Control" <??> (CacheControl.TryParse, CacheControl.Format)

        let connection =
            headersKey "Connection" <??> (Connection.TryParse, Connection.Format)

        let contentEncoding =
            headersKey "Content-Encoding" <??> (ContentEncoding.TryParse, ContentEncoding.Format)

        let contentLanguage =
            headersKey "Content-Language" <??> (ContentLanguage.TryParse, ContentLanguage.Format)

        let contentLength =
            headersKey "Content-Length" <??> (ContentLength.TryParse, ContentLength.Format)

        let contentLocation =
            headersKey "Content-Location" <??> (ContentLocation.TryParse, ContentLocation.Format)

        // TODO: typed ContentRange

        let contentRange =
            headersKey "Content-Range"

        let contentType =
            headersKey "Content-Type" <??> (ContentType.TryParse, ContentType.Format)

        let date =
            headersKey "Date" <??> (Date.TryParse, Date.Format)

        let eTag =
            headersKey "ETag" <??> (ETag.TryParse, ETag.Format)

        let expires =
            headersKey "Expires" <??> (Expires.TryParse, Expires.Format)

        let lastModified =
            headersKey "Last-Modified" <??> (LastModified.TryParse, LastModified.Format)

        let location =
            headersKey "Location" <??> (Location.TryParse, Location.Format)

        // TODO: typed ProxyAuthenticate

        let proxyAuthenticate =
            headersKey "Proxy-Authenticate"

        // TODO: typed RetryAfter

        let retryAfter =
            headersKey "Retry-After" <??> (RetryAfter.TryParse, RetryAfter.Format)

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
