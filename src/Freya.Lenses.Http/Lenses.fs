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

    let Body_ =
        Environment.Required_<Stream> Constants.requestBody

    let Headers_ =
        Environment.Required_<IDictionary<string, string []>> Constants.requestHeaders

    let Header_ key =
        Headers_ >-?> mutKeyP_<string, string []> key <?-> ((String.concat ","), (Array.create 1))

    let Method_ = 
        Environment.Required_<string> Constants.requestMethod <--> (Method.Parse, Method.Format)

    let Path_ =
        Environment.Required_<string> Constants.requestPath

    let PathBase_ =
        Environment.Required_<string> Constants.requestPathBase

    let HttpVersion_ =
        Environment.Required_<string> Constants.requestProtocol <--> (HttpVersion.Parse, HttpVersion.Format)

    let Scheme_ =
        Environment.Required_<string> Constants.requestScheme <--> (Scheme.Parse, Scheme.Format)

    let Query_ =
        Environment.Required_<string> Constants.requestQueryString <--> (Query.Parse, Query.Format)

    (* Request Header Lenses *)

    [<RequireQualifiedAccess>]
    module Headers =

        let private header_ key tryParse format =
            Header_ key <??> (tryParse, format)

        let Accept_ =
            header_ "Accept" Accept.TryParse Accept.Format

        let AcceptCharset_ =
            header_ "Accept-Charset" AcceptCharset.TryParse AcceptCharset.Format

        let AcceptEncoding_ =
            header_ "Accept-Encoding" AcceptEncoding.TryParse AcceptEncoding.Format

        let AcceptLanguage_ =
            header_ "Accept-Language" AcceptLanguage.TryParse AcceptLanguage.Format

        // TODO: typed Authorization

        let Authorization_ =
            Header_ "Authorization"

        let CacheControl_ =
            header_ "Cache-Control" CacheControl.TryParse CacheControl.Format

        let Connection_ =
            header_ "Connection" Connection.TryParse Connection.Format

        let ContentEncoding_ =
            header_ "Content-Encoding" ContentEncoding.TryParse ContentEncoding.Format

        let ContentLanguage_ =
            header_ "Content-Language" ContentLanguage.TryParse ContentLanguage.Format

        let ContentLength_ =
            header_ "Content-Length" ContentLength.TryParse, ContentLength.Format

        let ContentLocation_ =
            header_ "Content-Location" ContentLocation.TryParse, ContentLocation.Format

        let ContentType_ =
            header_ "Content-Type" ContentType.TryParse ContentType.Format

        let Date_ =
            header_ "Date" Date.TryParse Date.Format

        let Expect_ =
            header_ "Expect" Expect.TryParse Expect.Format

        // TODO: typed From

        let From_ =
            Header_ "From"

        let Host_ =
            header_ "Host" Host.TryParse Host.Format

        let IfMatch_ =
            header_ "If-Match" IfMatch.TryParse IfMatch.Format

        let IfModifiedSince_ =
            header_ "If-Modified-Since" IfModifiedSince.TryParse IfModifiedSince.Format

        let IfNoneMatch_ =
            header_ "If-None-Match" IfNoneMatch.TryParse IfNoneMatch.Format

        let IfRange_ =
            header_ "If-Range" IfRange.TryParse IfRange.Format

        let IfUnmodifiedSince_ =
            header_ "If-Unmodified-Since" IfUnmodifiedSince.TryParse IfUnmodifiedSince.Format

        let MaxForwards_ =
            header_ "Max-Forwards" MaxForwards.TryParse MaxForwards.Format

        // TODO: typed Pragma

        let Pragma_ =
            Header_ "Pragma"

        // TODO: typed ProxyAuthorization

        let ProxyAuthorization_ =
            Header_ "Proxy-Authorization"

        // TODO: typed Range

        let Range_ =
            Header_ "Range"

        let Referer_ =
            header_ "Referer" Referer.TryParse Referer.Format

        // TODO: typed TE

        let TE_ =
            Header_ "TE"

        // TODO: typed Trailer

        let Trailer_ =
            Header_ "Trailer"

        // TODO: typed TransferEncoding

        let TransferEncoding_ =
            Header_ "Transfer-Encoding"

        // TODO: typed Upgrade

        let Upgrade_ =
            Header_ "Upgrade"

        // TODO: typed UserAgent

        let UserAgent_ =
            Header_ "User-Agent"

        // TODO: typed Via

        let Via_ =
            Header_ "Via"

(* Response Lenses *)

[<RequireQualifiedAccess>]
module Response =

    let Body_ =
        Environment.Required_<Stream> Constants.responseBody

    let Headers_ =
        Environment.Required_<IDictionary<string, string []>> Constants.responseHeaders

    let Header_ key =
        Headers_ >-?> mutKeyP_<string, string []> key <?-> ((String.concat ","), (Array.create 1))

    let HttpVersion_ =
        Environment.Optional_<string> Constants.responseProtocol <?-> (HttpVersion.Parse, HttpVersion.Format)

    let ReasonPhrase_ =
        Environment.Optional_<string> Constants.responseReasonPhrase

    let StatusCode_ =
        Environment.Optional_<int> Constants.responseStatusCode

    (* Response Header Lenses *)

    [<RequireQualifiedAccess>]
    module Headers =

        let private header_ key tryParse format =
            Header_ key <??> (tryParse, format)

        // TODO: typed AcceptRanges

        let AcceptRanges_ =
            Header_ "Accept-Ranges"

        let Age_ =
            header_ "Age" Age.TryParse Age.Format

        let Allow_ =
            header_ "Allow" Allow.TryParse Allow.Format

        let CacheControl_ =
            header_ "Cache-Control" CacheControl.TryParse CacheControl.Format

        let Connection_ =
            header_ "Connection" Connection.TryParse Connection.Format

        let ContentEncoding_ =
            header_ "Content-Encoding" ContentEncoding.TryParse ContentEncoding.Format

        let ContentLanguage_ =
            header_ "Content-Language" ContentLanguage.TryParse ContentLanguage.Format

        let ContentLength_ =
            header_ "Content-Length" ContentLength.TryParse ContentLength.Format

        let ContentLocation_ =
            header_ "Content-Location" ContentLocation.TryParse ContentLocation.Format

        // TODO: typed ContentRange

        let ContentRange_ =
            Header_ "Content-Range"

        let ContentType_ =
            header_ "Content-Type" ContentType.TryParse ContentType.Format

        let Date_ =
            header_ "Date" Date.TryParse Date.Format

        let ETag_ =
            header_ "ETag" ETag.TryParse ETag.Format

        let Expires_ =
            header_ "Expires" Expires.TryParse Expires.Format

        let LastModified_ =
            header_ "Last-Modified" LastModified.TryParse LastModified.Format

        let Location_ =
            header_ "Location" Location.TryParse Location.Format

        // TODO: typed ProxyAuthenticate

        let ProxyAuthenticate_ =
            Header_ "Proxy-Authenticate"

        // TODO: typed RetryAfter

        let RetryAfter_ =
            header_ "Retry-After" RetryAfter.TryParse RetryAfter.Format

        // TODO: typed Server

        let Server_ =
            Header_ "Server"

        // TODO: typed Trailer

        let Trailer_ =
            Header_ "Trailer"

        // TODO: typed TransferEncoding

        let TransferEncoding_ =
            Header_ "Transfer-Encoding"

        // TODO: typed Upgrade

        let Upgrade_ =
            Header_ "Upgrade"

        // TODO: typed Vary

        let Vary_ =
            Header_ "Vary"

        // TODO: typed Warning

        let Warning_ =
            Header_ "Warning"

        // TODO: typed WWWAuthenticate

        let WwwAuthenticate_ =
            Header_ "WWW-Authenticate"