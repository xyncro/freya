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
//
//----------------------------------------------------------------------------

[<AutoOpen>]
module Freya.Lenses.Http.Lenses

open System.Collections.Generic
open System.IO
open Aether.Operators
open Arachne.Http
open Arachne.Uri
open Freya.Core

(* Request Lenses *)

[<RequireQualifiedAccess>]
module Request =

    let body_ =
            Environment.value_<Stream> Constants.requestBody
       <--> Option.unsafe_

    let headers_ =
            Environment.value_<IDictionary<string, string []>> Constants.requestHeaders
       <--> Option.unsafe_

    let header_ key =
            headers_
       >--> Dict.value_<string, string []> key
       <--> Option.mapIsomorphism ((String.concat ","), (Array.create 1))

    let method_ = 
            Environment.value_<string> Constants.requestMethod
       <--> Option.unsafe_
       <--> (Method.Parse, Method.Format)

    let path_ =
            Environment.value_<string> Constants.requestPath
       <--> Option.unsafe_

    let pathBase_ =
            Environment.value_<string> Constants.requestPathBase
       <--> Option.unsafe_

    let httpVersion_ =
            Environment.value_<string> Constants.requestProtocol
       <--> Option.unsafe_
       <--> (HttpVersion.Parse, HttpVersion.Format)

    let scheme_ =
            Environment.value_<string> Constants.requestScheme
       <--> Option.unsafe_
       <--> (Scheme.Parse, Scheme.Format)

    let query_ =
            Environment.value_<string> Constants.requestQueryString
       <--> Option.unsafe_
       <--> (Query.Parse, Query.Format)

    (* Request Header Lenses *)

    [<RequireQualifiedAccess>]
    module Headers =

        let private value_ key e =
                header_ key
           <--> Option.mapEpimorphism e

        let accept_ =
            value_
                "Accept"
                (Accept.TryParse, Accept.Format)

        let acceptCharset_ =
            value_
                "Accept-Charset"
                (AcceptCharset.TryParse, AcceptCharset.Format)

        let acceptEncoding_ =
            value_
                "Accept-Encoding"
                (AcceptEncoding.TryParse, AcceptEncoding.Format)

        let acceptLanguage_ =
            value_
                "Accept-Language"
                (AcceptLanguage.TryParse, AcceptLanguage.Format)

        // TODO: typed Authorization

        let authorization_ =
            header_ "Authorization"

        let cacheControl_ =
            value_
                "Cache-Control"
                (CacheControl.TryParse, CacheControl.Format)

        let connection_ =
            value_
                "Connection"
                (Connection.TryParse, Connection.Format)

        let contentEncoding_ =
            value_
                "Content-Encoding"
                (ContentEncoding.TryParse, ContentEncoding.Format)

        let contentLanguage_ =
            value_
                "Content-Language"
                (ContentLanguage.TryParse, ContentLanguage.Format)

        let contentLength_ =
            value_
                "Content-Length"
                (ContentLength.TryParse, ContentLength.Format)

        let contentLocation_ =
            value_
                "Content-Location"
                (ContentLocation.TryParse, ContentLocation.Format)

        let contentType_ =
            value_
                "Content-Type"
                (ContentType.TryParse, ContentType.Format)

        let date_ =
            value_
                "Date"
                (Date.TryParse, Date.Format)

        let expect_ =
            value_
                "Expect"
                (Expect.TryParse, Expect.Format)

        // TODO: typed From

        let from_ =
            header_ "From"

        let host_ =
            value_
                "Host"
                (Host.TryParse, Host.Format)

        let ifMatch_ =
            value_
                "If-Match"
                (IfMatch.TryParse, IfMatch.Format)

        let ifModifiedSince_ =
            value_
                "If-Modified-Since"
                (IfModifiedSince.TryParse, IfModifiedSince.Format)

        let ifNoneMatch_ =
            value_
                "If-None-Match"
                (IfNoneMatch.TryParse, IfNoneMatch.Format)

        let ifRange_ =
            value_
                "If-Range"
                (IfRange.TryParse, IfRange.Format)

        let ifUnmodifiedSince_ =
            value_
                "If-Unmodified-Since"
                (IfUnmodifiedSince.TryParse, IfUnmodifiedSince.Format)

        let maxForwards_ =
            value_
                "Max-Forwards"
                (MaxForwards.TryParse, MaxForwards.Format)

        // TODO: typed Pragma

        let pragma_ =
            header_ "Pragma"

        // TODO: typed ProxyAuthorization

        let proxyAuthorization_ =
            header_ "Proxy-Authorization"

        // TODO: typed Range

        let range_ =
            header_ "Range"

        let referer_ =
            value_
                "Referer"
                (Referer.TryParse, Referer.Format)

        // TODO: typed TE

        let te_ =
            header_ "TE"

        // TODO: typed Trailer

        let trailer_ =
            header_ "Trailer"

        // TODO: typed TransferEncoding

        let transferEncoding_ =
            header_ "Transfer-Encoding"

        // TODO: typed Upgrade

        let upgrade_ =
            header_ "Upgrade"

        // TODO: typed UserAgent

        let userAgent_ =
            header_ "User-Agent"

        // TODO: typed Via

        let via_ =
            header_ "Via"

(* Response Lenses *)

[<RequireQualifiedAccess>]
module Response =

    let body_ =
            Environment.value_<Stream> Constants.responseBody
       <--> Option.unsafe_

    let headers_ =
            Environment.value_<IDictionary<string, string []>> Constants.responseHeaders
       <--> Option.unsafe_

    let header_ key =
            headers_
       >--> Dict.value_<string, string []> key
       <--> Option.mapIsomorphism ((String.concat ","), (Array.create 1))

    let httpVersion_ =
            Environment.value_<string> Constants.responseProtocol
       <--> Option.mapIsomorphism (HttpVersion.Parse, HttpVersion.Format)

    let reasonPhrase_ =
            Environment.value_<string> Constants.responseReasonPhrase

    let statusCode_ =
            Environment.value_<int> Constants.responseStatusCode

    (* Response Header Lenses *)

    [<RequireQualifiedAccess>]
    module Headers =

        let private value_ key e =
                header_ key
           <--> Option.mapEpimorphism e

        // TODO: typed AcceptRanges

        let acceptRanges_ =
            header_ "Accept-Ranges"

        let age_ =
            value_
                "Age"
                (Age.TryParse, Age.Format)

        let allow_ =
            value_
                "Allow"
                (Allow.TryParse, Allow.Format)

        let cacheControl_ =
            value_
                "Cache-Control"
                (CacheControl.TryParse, CacheControl.Format)

        let connection_ =
            value_
                "Connection"
                (Connection.TryParse, Connection.Format)

        let contentEncoding_ =
            value_
                "Content-Encoding"
                (ContentEncoding.TryParse, ContentEncoding.Format)

        let contentLanguage_ =
            value_
                "Content-Language"
                (ContentLanguage.TryParse, ContentLanguage.Format)

        let contentLength_ =
            value_
                "Content-Length"
                (ContentLength.TryParse, ContentLength.Format)

        let contentLocation_ =
            value_
                "Content-Location"
                (ContentLocation.TryParse, ContentLocation.Format)

        // TODO: typed ContentRange

        let contentRange_ =
            header_ "Content-Range"

        let contentType_ =
            value_
                "Content-Type"
                (ContentType.TryParse, ContentType.Format)

        let date_ =
            value_
                "Date"
                (Date.TryParse, Date.Format)

        let eTag_ =
            value_
                "ETag"
                (ETag.TryParse, ETag.Format)

        let expires_ =
            value_
                "Expires"
                (Expires.TryParse, Expires.Format)

        let lastModified_ =
            value_
                "Last-Modified"
                (LastModified.TryParse, LastModified.Format)

        let location_ =
            value_
                "Location"
                (Location.TryParse, Location.Format)

        // TODO: typed ProxyAuthenticate

        let proxyAuthenticate_ =
            header_ "Proxy-Authenticate"

        // TODO: typed RetryAfter

        let retryAfter_ =
            value_
                "Retry-After"
                (RetryAfter.TryParse, RetryAfter.Format)

        // TODO: typed Server

        let server_ =
            header_ "Server"

        // TODO: typed Trailer

        let trailer_ =
            header_ "Trailer"

        // TODO: typed TransferEncoding

        let transferEncoding_ =
            header_ "Transfer-Encoding"

        // TODO: typed Upgrade

        let upgrade_ =
            header_ "Upgrade"

        // TODO: typed Vary

        let vary_ =
            header_ "Vary"

        // TODO: typed Warning

        let warning_ =
            header_ "Warning"

        // TODO: typed WWWAuthenticate

        let wwwAuthenticate_ =
            header_ "WWW-Authenticate"