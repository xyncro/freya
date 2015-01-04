//----------------------------------------------------------------------------
//
// Copyright (c) 2014
//
//    Ryan Riley (@panesofglass) and Andrew Cherry (@kolektiv)
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//----------------------------------------------------------------------------

[<AutoOpen>]
module Freya.Types.Http.headerPIsoes

open System.Collections.Generic
open System.IO
open Aether.Operators
open Freya.Core

(* Request Lenses *)

[<RequireQualifiedAccess>]
module Request =

    let body =
        environmentKeyLens<Stream> Constants.requestBody

    let headers =
        environmentKeyLens<IDictionary<string, string []>> Constants.requestHeaders

    let headersKey key =
        headers >-?> mutDictPLens<string, string []> key <?-> ((String.concat ","), (Array.create 1))

    let meth = 
        environmentKeyLens<string> Constants.requestMethod <--> (Method.Parse, Method.Format)

    let path = 
        environmentKeyLens<string> Constants.requestPath

    let pathBase =
        environmentKeyLens<string> Constants.requestPathBase

    let httpVersion =
        environmentKeyLens<string> Constants.requestProtocol <--> (HttpVersion.Parse, HttpVersion.Format)

    let scheme =
        environmentKeyLens<string> Constants.requestScheme

    let query =
        environmentKeyLens<string> Constants.requestQueryString // <--> TODO: Isomorphism

// TODO: Reinstate when query is iso again

//    let queryKey key =
//        query >-?> mapPheaderPIso key

    (* Request Header Lenses *)

    [<RequireQualifiedAccess>]
    module Headers =

        let private headerPIso key tryParse format =
            headersKey key <??> (tryParse, format)

        let accept =
            headerPIso "Accept" Accept.TryParse Accept.Format

        let acceptCharset =
            headerPIso "Accept-Charset" AcceptCharset.TryParse AcceptCharset.Format

        let acceptEncoding =
            headerPIso "Accept-Encoding" AcceptEncoding.TryParse AcceptEncoding.Format

        let acceptLanguage =
            headerPIso "Accept-Language" AcceptLanguage.TryParse AcceptLanguage.Format

        // TODO: typed Authorization

        let authorization =
            headersKey "Authorization"

        let cacheControl =
            headerPIso "Cache-Control" CacheControl.TryParse CacheControl.Format

        let connection =
            headerPIso "Connection" Connection.TryParse Connection.Format

        let contentEncoding =
            headerPIso "Content-Encoding" ContentEncoding.TryParse ContentEncoding.Format

        let contentLanguage =
            headerPIso "Content-Language" ContentLanguage.TryParse ContentLanguage.Format

        let contentLength =
            headerPIso "Content-Length" ContentLength.TryParse, ContentLength.Format

        let contentLocation =
            headerPIso "Content-Location" ContentLocation.TryParse, ContentLocation.Format

        let contentType =
            headerPIso "Content-Type" ContentType.TryParse ContentType.Format

        let date =
            headerPIso "Date" Date.TryParse Date.Format

        let expect =
            headerPIso "Expect" Expect.TryParse Expect.Format

        // TODO: typed From

        let from =
            headersKey "From"

        let host =
            headerPIso "Host" Host.TryParse Host.Format

        let ifMatch =
            headerPIso "If-Match" IfMatch.TryParse IfMatch.Format

        let ifModifiedSince =
            headerPIso "If-Modified-Since" IfModifiedSince.TryParse IfModifiedSince.Format

        let ifNoneMatch =
            headerPIso "If-None-Match" IfNoneMatch.TryParse IfNoneMatch.Format

        let ifRange =
            headerPIso "If-Range" IfRange.TryParse IfRange.Format

        let ifUnmodifiedSince =
            headerPIso "If-Unmodified-Since" IfUnmodifiedSince.TryParse IfUnmodifiedSince.Format

        let maxForwards =
            headerPIso "Max-Forwards" MaxForwards.TryParse MaxForwards.Format

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
            headerPIso "Referer" Referer.TryParse Referer.Format

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
        environmentKeyLens<Stream> Constants.responseBody

    let headers =
        environmentKeyLens<IDictionary<string, string []>> Constants.responseHeaders

    let headersKey key =
        headers >-?> mutDictPLens<string, string []> key <?-> ((String.concat ","), (Array.create 1))

    let httpVersion =
        environmentKeyPLens<string> Constants.responseProtocol <?-> (HttpVersion.Parse, HttpVersion.Format)

    let reasonPhrase =
        environmentKeyPLens<string> Constants.responseReasonPhrase

    let statusCode =
        environmentKeyPLens<int> Constants.responseStatusCode

    (* Response Header Lenses *)

    [<RequireQualifiedAccess>]
    module Headers =

        let private headerPIso key tryParse format =
            headersKey key <??> (tryParse, format)

        // TODO: typed AcceptRanges

        let acceptRanges =
            headersKey "Accept-Ranges"

        let age =
            headerPIso "Age" Age.TryParse Age.Format

        let allow =
            headerPIso "Allow" Allow.TryParse Allow.Format

        let cacheControl =
            headerPIso "Cache-Control" CacheControl.TryParse CacheControl.Format

        let connection =
            headerPIso "Connection" Connection.TryParse Connection.Format

        let contentEncoding =
            headerPIso "Content-Encoding" ContentEncoding.TryParse ContentEncoding.Format

        let contentLanguage =
            headerPIso "Content-Language" ContentLanguage.TryParse ContentLanguage.Format

        let contentLength =
            headerPIso "Content-Length" ContentLength.TryParse ContentLength.Format

        let contentLocation =
            headerPIso "Content-Location" ContentLocation.TryParse ContentLocation.Format

        // TODO: typed ContentRange

        let contentRange =
            headersKey "Content-Range"

        let contentType =
            headerPIso "Content-Type" ContentType.TryParse ContentType.Format

        let date =
            headerPIso "Date" Date.TryParse Date.Format

        let eTag =
            headerPIso "ETag" ETag.TryParse ETag.Format

        let expires =
            headerPIso "Expires" Expires.TryParse Expires.Format

        let lastModified =
            headerPIso "Last-Modified" LastModified.TryParse LastModified.Format

        let location =
            headerPIso "Location" Location.TryParse Location.Format

        // TODO: typed ProxyAuthenticate

        let proxyAuthenticate =
            headersKey "Proxy-Authenticate"

        // TODO: typed RetryAfter

        let retryAfter =
            headerPIso "Retry-After" RetryAfter.TryParse RetryAfter.Format

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