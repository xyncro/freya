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

open System
open System.Collections.Generic
open System.IO
open Aether.Operators
open Arachne.Http
open Arachne.Uri
open Freya.Core

(* Obsolete

   Backwards compatibility shims to make the 2.x-> 3.x transition
   less painful, providing functionally equivalent options where possible.

   To be removed for 4.x releases. *)

let private option_ =
    id, Some

(* Request Lenses *)

[<RequireQualifiedAccess>]
module Request =

    let body_ =
            Environment.value_<Stream> Constants.requestBody
        >-> Option.unsafe_

    let headers_ =
            Environment.value_<IDictionary<string, string []>> Constants.requestHeaders
        >-> Option.unsafe_

    let header_ key =
            headers_
        >-> Dict.value_<string, string []> key
        >-> Option.mapIsomorphism ((String.concat ","), (Array.create 1))

    let method_ = 
            Environment.value_<string> Constants.requestMethod
        >-> Option.unsafe_
        >-> (Method.parse, Method.format)

    let path_ =
            Environment.value_<string> Constants.requestPath
        >-> Option.unsafe_

    let pathBase_ =
            Environment.value_<string> Constants.requestPathBase
        >-> Option.unsafe_

    let httpVersion_ =
            Environment.value_<string> Constants.requestProtocol
        >-> Option.unsafe_
        >-> (HttpVersion.parse, HttpVersion.format)

    let scheme_ =
            Environment.value_<string> Constants.requestScheme
        >-> Option.unsafe_
        >-> (Scheme.parse, Scheme.format)

    let query_ =
            Environment.value_<string> Constants.requestQueryString
        >-> Option.unsafe_
        >-> (Query.parse, Query.format)

    (* Obsolete

       Backwards compatibility shims to make the 2.x-> 3.x transition
       less painful, providing functionally equivalent options where possible.

       To be removed for 4.x releases. *)

    [<Obsolete ("Use Request.body_ instead.")>]
    let Body_ =
        body_

    [<Obsolete ("Use Request.headers_ instead.")>]
    let Headers_ =
        headers_

    [<Obsolete ("Use Request.header_ instead.")>]
    let Header_ key =
            header_ key
        >-> option_

    [<Obsolete ("Use Request.method_ instead.")>]
    let Method_ =
        method_

    [<Obsolete ("Use Request.path_ instead.")>]
    let Path_ =
        path_

    [<Obsolete ("Use Request.pathBase_ instead.")>]
    let PathBase_ =
        pathBase_

    [<Obsolete ("Use Request.httpVersion_ instead.")>]
    let HttpVersion_ =
        httpVersion_

    [<Obsolete ("Use Request.scheme_ instead.")>]
    let Scheme_ =
        scheme_

    [<Obsolete ("Use Request.query_ instead.")>]
    let Query_ =
        query_

    (* Request Header Lenses *)

    [<RequireQualifiedAccess>]
    module Headers =

        let private value_ key e =
                header_ key
            >-> Option.mapEpimorphism e

        let accept_ =
            value_
                "Accept"
                (Accept.tryParse >> Option.ofChoice, Accept.format)

        let acceptCharset_ =
            value_
                "Accept-Charset"
                (AcceptCharset.tryParse >> Option.ofChoice, AcceptCharset.format)

        let acceptEncoding_ =
            value_
                "Accept-Encoding"
                (AcceptEncoding.tryParse >> Option.ofChoice, AcceptEncoding.format)

        let acceptLanguage_ =
            value_
                "Accept-Language"
                (AcceptLanguage.tryParse >> Option.ofChoice, AcceptLanguage.format)

        // TODO: typed Authorization

        let authorization_ =
            header_ "Authorization"

        let cacheControl_ =
            value_
                "Cache-Control"
                (CacheControl.tryParse >> Option.ofChoice, CacheControl.format)

        let connection_ =
            value_
                "Connection"
                (Connection.tryParse >> Option.ofChoice, Connection.format)

        let contentEncoding_ =
            value_
                "Content-Encoding"
                (ContentEncoding.tryParse >> Option.ofChoice, ContentEncoding.format)

        let contentLanguage_ =
            value_
                "Content-Language"
                (ContentLanguage.tryParse >> Option.ofChoice, ContentLanguage.format)

        let contentLength_ =
            value_
                "Content-Length"
                (ContentLength.tryParse >> Option.ofChoice, ContentLength.format)

        let contentLocation_ =
            value_
                "Content-Location"
                (ContentLocation.tryParse >> Option.ofChoice, ContentLocation.format)

        let contentType_ =
            value_
                "Content-Type"
                (ContentType.tryParse >> Option.ofChoice, ContentType.format)

        let date_ =
            value_
                "Date"
                (Date.tryParse >> Option.ofChoice, Date.format)

        let expect_ =
            value_
                "Expect"
                (Expect.tryParse >> Option.ofChoice, Expect.format)

        // TODO: typed From

        let from_ =
            header_ "From"

        let host_ =
            value_
                "Host"
                (Host.tryParse >> Option.ofChoice, Host.format)

        let ifMatch_ =
            value_
                "If-Match"
                (IfMatch.tryParse >> Option.ofChoice, IfMatch.format)

        let ifModifiedSince_ =
            value_
                "If-Modified-Since"
                (IfModifiedSince.tryParse >> Option.ofChoice, IfModifiedSince.format)

        let ifNoneMatch_ =
            value_
                "If-None-Match"
                (IfNoneMatch.tryParse >> Option.ofChoice, IfNoneMatch.format)

        let ifRange_ =
            value_
                "If-Range"
                (IfRange.tryParse >> Option.ofChoice, IfRange.format)

        let ifUnmodifiedSince_ =
            value_
                "If-Unmodified-Since"
                (IfUnmodifiedSince.tryParse >> Option.ofChoice, IfUnmodifiedSince.format)

        let maxForwards_ =
            value_
                "Max-Forwards"
                (MaxForwards.tryParse >> Option.ofChoice, MaxForwards.format)

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
                (Referer.tryParse >> Option.ofChoice, Referer.format)

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

        (* Obsolete

           Backwards compatibility shims to make the 2.x-> 3.x transition
           less painful, providing functionally equivalent options where possible.

           To be removed for 4.x releases. *)

        [<Obsolete ("Use Request.Headers.accept_ instead.")>]
        let Accept_ =
                accept_
            >-> option_

        [<Obsolete ("Use Request.Headers.acceptCharset_ instead.")>]
        let AcceptCharset_ =
                acceptCharset_
            >-> option_

        [<Obsolete ("Use Request.Headers.acceptEncoding_ instead.")>]
        let AcceptEncoding_ =
                acceptEncoding_
            >-> option_

        [<Obsolete ("Use Request.Headers.acceptLanguage_ instead.")>]
        let AcceptLanguage_ =
                acceptLanguage_
            >-> option_

        [<Obsolete ("Use Request.Headers.authorization_ instead.")>]
        let Authorization_ =
                authorization_
            >-> option_

        [<Obsolete ("Use Request.Headers.cacheControl_ instead.")>]
        let CacheControl_ =
                cacheControl_
            >-> option_

        [<Obsolete ("Use Request.Headers.connection_ instead.")>]
        let Connection_ =
                connection_
            >-> option_

        [<Obsolete ("Use Request.Headers.contentEncoding_ instead.")>]
        let ContentEncoding_ =
                contentEncoding_
            >-> option_

        [<Obsolete ("Use Request.Headers.contentLanguage_ instead.")>]
        let ContentLanguage_ =
                contentLanguage_
            >-> option_

        [<Obsolete ("Use Request.Headers.contentLength_ instead.")>]
        let ContentLength_ =
                contentLength_
            >-> option_

        [<Obsolete ("Use Request.Headers.contentLocation_ instead.")>]
        let ContentLocation_ =
                contentLocation_
            >-> option_

        [<Obsolete ("Use Request.Headers.contentType_ instead.")>]
        let ContentType_ =
                contentType_
            >-> option_

        [<Obsolete ("Use Request.Headers.date_ instead.")>]
        let Date_ =
                date_
            >-> option_

        [<Obsolete ("Use Request.Headers.expect_ instead.")>]
        let Expect_ =
                expect_
            >-> option_

        [<Obsolete ("Use Request.Headers.from_ instead.")>]
        let From_ =
                from_
            >-> option_

        [<Obsolete ("Use Request.Headers.host_ instead.")>]
        let Host_ =
                host_
            >-> option_

        [<Obsolete ("Use Request.Headers.ifMatch_ instead.")>]
        let IfMatch_ =
                ifMatch_
            >-> option_

        [<Obsolete ("Use Request.Headers.ifModifiedSince_ instead.")>]
        let IfModifiedSince_ =
                ifModifiedSince_
            >-> option_

        [<Obsolete ("Use Request.Headers.ifNoneMatch_ instead.")>]
        let IfNoneMatch_ =
                ifNoneMatch_
            >-> option_

        [<Obsolete ("Use Request.Headers.ifRange_ instead.")>]
        let IfRange_ =
                ifRange_
            >-> option_

        [<Obsolete ("Use Request.Headers.ifUnmodifiedSince_ instead.")>]
        let IfUnmodifiedSince_ =
                ifUnmodifiedSince_
            >-> option_

        [<Obsolete ("Use Request.Headers.maxForwards_ instead.")>]
        let MaxForwards_ =
                maxForwards_
            >-> option_

        [<Obsolete ("Use Request.Headers.pragma_ instead.")>]
        let Pragma_ =
                pragma_
            >-> option_

        [<Obsolete ("Use Request.Headers.proxyAuthorization_ instead.")>]
        let ProxyAuthorization_ =
                proxyAuthorization_
            >-> option_

        [<Obsolete ("Use Request.Headers.range_ instead.")>]
        let Range_ =
                range_
            >-> option_

        [<Obsolete ("Use Request.Headers.referer_ instead.")>]
        let Referer_ =
                referer_
            >-> option_

        [<Obsolete ("Use Request.Headers.te_ instead.")>]
        let TE_ =
                te_
            >-> option_

        [<Obsolete ("Use Request.Headers.trailer_ instead.")>]
        let Trailer_ =
                trailer_
            >-> option_

        [<Obsolete ("Use Request.Headers.transferEncoding_ instead.")>]
        let TransferEncoding_ =
                transferEncoding_
            >-> option_

        [<Obsolete ("Use Request.Headers.upgrade_ instead.")>]
        let Upgrade_ =
                upgrade_
            >-> option_

        [<Obsolete ("Use Request.Headers.userAgent_ instead.")>]
        let UserAgent_ =
                userAgent_
            >-> option_

        [<Obsolete ("Use Request.Headers.via_ instead.")>]
        let Via_ =
                via_
            >-> option_

(* Response Lenses *)

[<RequireQualifiedAccess>]
module Response =

    let body_ =
            Environment.value_<Stream> Constants.responseBody
        >-> Option.unsafe_

    let headers_ =
            Environment.value_<IDictionary<string, string []>> Constants.responseHeaders
        >-> Option.unsafe_

    let header_ key =
            headers_
        >-> Dict.value_<string, string []> key
        >-> Option.mapIsomorphism ((String.concat ","), (Array.create 1))

    let httpVersion_ =
            Environment.value_<string> Constants.responseProtocol
        >-> Option.mapIsomorphism (HttpVersion.parse, HttpVersion.format)

    let reasonPhrase_ =
            Environment.value_<string> Constants.responseReasonPhrase

    let statusCode_ =
            Environment.value_<int> Constants.responseStatusCode

    (* Obsolete

       Backwards compatibility shims to make the 2.x-> 3.x transition
       less painful, providing functionally equivalent options where possible.

       To be removed for 4.x releases. *)

    [<Obsolete ("Use Response.body_ instead.")>]
    let Body_ =
        body_

    [<Obsolete ("Use Response.headers_ instead.")>]
    let Headers_ =
        headers_

    [<Obsolete ("Use Response.header_ instead.")>]
    let Header_ key =
            header_ key
        >-> option_

    [<Obsolete ("Use Response.httpVersion_ instead.")>]
    let HttpVersion_ =
            httpVersion_
        >-> option_

    [<Obsolete ("Use Response.reasonPhrase_ instead.")>]
    let ReasonPhrase_ =
            reasonPhrase_
        >-> option_

    [<Obsolete ("Use Response.statusCode_ instead.")>]
    let StatusCode_ =
            statusCode_
        >-> option_

    (* Response Header Lenses *)

    [<RequireQualifiedAccess>]
    module Headers =

        let private value_ key e =
                header_ key
            >-> Option.mapEpimorphism e

        // TODO: typed AcceptRanges

        let acceptRanges_ =
            header_ "Accept-Ranges"

        let age_ =
            value_
                "Age"
                (Age.tryParse >> Option.ofChoice, Age.format)

        let allow_ =
            value_
                "Allow"
                (Allow.tryParse >> Option.ofChoice, Allow.format)

        let cacheControl_ =
            value_
                "Cache-Control"
                (CacheControl.tryParse >> Option.ofChoice, CacheControl.format)

        let connection_ =
            value_
                "Connection"
                (Connection.tryParse >> Option.ofChoice, Connection.format)

        let contentEncoding_ =
            value_
                "Content-Encoding"
                (ContentEncoding.tryParse >> Option.ofChoice, ContentEncoding.format)

        let contentLanguage_ =
            value_
                "Content-Language"
                (ContentLanguage.tryParse >> Option.ofChoice, ContentLanguage.format)

        let contentLength_ =
            value_
                "Content-Length"
                (ContentLength.tryParse >> Option.ofChoice, ContentLength.format)

        let contentLocation_ =
            value_
                "Content-Location"
                (ContentLocation.tryParse >> Option.ofChoice, ContentLocation.format)

        // TODO: typed ContentRange

        let contentRange_ =
            header_ "Content-Range"

        let contentType_ =
            value_
                "Content-Type"
                (ContentType.tryParse >> Option.ofChoice, ContentType.format)

        let date_ =
            value_
                "Date"
                (Date.tryParse >> Option.ofChoice, Date.format)

        let eTag_ =
            value_
                "ETag"
                (ETag.tryParse >> Option.ofChoice, ETag.format)

        let expires_ =
            value_
                "Expires"
                (Expires.tryParse >> Option.ofChoice, Expires.format)

        let lastModified_ =
            value_
                "Last-Modified"
                (LastModified.tryParse >> Option.ofChoice, LastModified.format)

        let location_ =
            value_
                "Location"
                (Location.tryParse >> Option.ofChoice, Location.format)

        // TODO: typed ProxyAuthenticate

        let proxyAuthenticate_ =
            header_ "Proxy-Authenticate"

        // TODO: typed RetryAfter

        let retryAfter_ =
            value_
                "Retry-After"
                (RetryAfter.tryParse >> Option.ofChoice, RetryAfter.format)

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

        (* Obsolete

           Backwards compatibility shims to make the 2.x-> 3.x transition
           less painful, providing functionally equivalent options where possible.

           To be removed for 4.x releases. *)

        [<Obsolete ("Use Response.Headers.acceptRanges_ instead.")>]
        let AcceptRanges_ =
                acceptRanges_
            >-> option_

        [<Obsolete ("Use Response.Headers.age_ instead.")>]
        let Age_ =
                age_
            >-> option_

        [<Obsolete ("Use Response.Headers.allow_ instead.")>]
        let Allow_ =
                allow_
            >-> option_

        [<Obsolete ("Use Response.Headers.cacheControl_ instead.")>]
        let CacheControl_ =
                cacheControl_
            >-> option_

        [<Obsolete ("Use Response.Headers.connection_ instead.")>]
        let Connection_ =
                connection_
            >-> option_

        [<Obsolete ("Use Response.Headers.contentEncoding_ instead.")>]
        let ContentEncoding_ =
                contentEncoding_
            >-> option_

        [<Obsolete ("Use Response.Headers.contentLanguage_ instead.")>]
        let ContentLanguage_ =
                contentLanguage_
            >-> option_

        [<Obsolete ("Use Response.Headers.contentLength_ instead.")>]
        let ContentLength_ =
                contentLength_
            >-> option_

        [<Obsolete ("Use Response.Headers.contentLocation_ instead.")>]
        let ContentLocation_ =
                contentLocation_
            >-> option_

        [<Obsolete ("Use Response.Headers.contentRange_ instead.")>]
        let ContentRange_ =
                contentRange_
            >-> option_

        [<Obsolete ("Use Response.Headers.contentType_ instead.")>]
        let ContentType_ =
                contentType_
            >-> option_

        [<Obsolete ("Use Response.Headers.date_ instead.")>]
        let Date_ =
                date_
            >-> option_

        [<Obsolete ("Use Response.Headers.eTag_ instead.")>]
        let ETag_ =
                eTag_
            >-> option_

        [<Obsolete ("Use Response.Headers.expires_ instead.")>]
        let Expires_ =
                expires_
            >-> option_

        [<Obsolete ("Use Response.Headers.lastModified_ instead.")>]
        let LastModified_ =
                lastModified_
            >-> option_

        [<Obsolete ("Use Response.Headers.location_ instead.")>]
        let Location_ =
                location_
            >-> option_

        [<Obsolete ("Use Response.Headers.proxyAuthenticate_ instead.")>]
        let ProxyAuthenticate_ =
                proxyAuthenticate_
            >-> option_

        [<Obsolete ("Use Response.Headers.retryAfter_ instead.")>]
        let RetryAfter_ =
                retryAfter_
            >-> option_

        [<Obsolete ("Use Response.Headers.server_ instead.")>]
        let Server_ =
                server_
            >-> option_

        [<Obsolete ("Use Response.Headers.trailer_ instead.")>]
        let Trailer_ =
                trailer_
            >-> option_

        [<Obsolete ("Use Response.Headers.transferEncoding_ instead.")>]
        let TransferEncoding_ =
                transferEncoding_
            >-> option_

        [<Obsolete ("Use Response.Headers.upgrade_ instead.")>]
        let Upgrade_ =
                upgrade_
            >-> option_

        [<Obsolete ("Use Response.Headers.vary_ instead.")>]
        let Vary_ =
                vary_
            >-> option_

        [<Obsolete ("Use Response.Headers.warning_ instead.")>]
        let Warning_ =
                warning_
            >-> option_

        [<Obsolete ("Use Response.Headers.wwwAuthenticate_ instead.")>]
        let WwwAuthenticate_ =
                wwwAuthenticate_
            >-> option_