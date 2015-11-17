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
            Environment.value_<Stream> Constants.requestBody
       <--> Option.unsafe_

    let Headers_ =
            Environment.value_<IDictionary<string, string []>> Constants.requestHeaders
       <--> Option.unsafe_

    let Header_ key =
            Headers_
       >--> Dict.value_<string, string []> key
       <--> Option.mapIsomorphism ((String.concat ","), (Array.create 1))

    let Method_ = 
            Environment.value_<string> Constants.requestMethod
       <--> Option.unsafe_
       <--> (Method.Parse, Method.Format)

    let Path_ =
            Environment.value_<string> Constants.requestPath
       <--> Option.unsafe_

    let PathBase_ =
            Environment.value_<string> Constants.requestPathBase
       <--> Option.unsafe_

    let HttpVersion_ =
            Environment.value_<string> Constants.requestProtocol
       <--> Option.unsafe_
       <--> (HttpVersion.Parse, HttpVersion.Format)

    let Scheme_ =
            Environment.value_<string> Constants.requestScheme
       <--> Option.unsafe_
       <--> (Scheme.Parse, Scheme.Format)

    let Query_ =
            Environment.value_<string> Constants.requestQueryString
       <--> Option.unsafe_
       <--> (Query.Parse, Query.Format)

    (* Request Header Lenses *)

    [<RequireQualifiedAccess>]
    module Headers =

        let private value_ key e =
                Header_ key
           <--> Option.mapEpimorphism e

        let Accept_ =
            value_
                "Accept"
                (Accept.TryParse, Accept.Format)

        let AcceptCharset_ =
            value_
                "Accept-Charset"
                (AcceptCharset.TryParse, AcceptCharset.Format)

        let AcceptEncoding_ =
            value_
                "Accept-Encoding"
                (AcceptEncoding.TryParse, AcceptEncoding.Format)

        let AcceptLanguage_ =
            value_
                "Accept-Language"
                (AcceptLanguage.TryParse, AcceptLanguage.Format)

        // TODO: typed Authorization

        let Authorization_ =
            Header_ "Authorization"

        let CacheControl_ =
            value_
                "Cache-Control"
                (CacheControl.TryParse, CacheControl.Format)

        let Connection_ =
            value_
                "Connection"
                (Connection.TryParse, Connection.Format)

        let ContentEncoding_ =
            value_
                "Content-Encoding"
                (ContentEncoding.TryParse, ContentEncoding.Format)

        let ContentLanguage_ =
            value_
                "Content-Language"
                (ContentLanguage.TryParse, ContentLanguage.Format)

        let ContentLength_ =
            value_
                "Content-Length"
                (ContentLength.TryParse, ContentLength.Format)

        let ContentLocation_ =
            value_
                "Content-Location"
                (ContentLocation.TryParse, ContentLocation.Format)

        let ContentType_ =
            value_
                "Content-Type"
                (ContentType.TryParse, ContentType.Format)

        let Date_ =
            value_
                "Date"
                (Date.TryParse, Date.Format)

        let Expect_ =
            value_
                "Expect"
                (Expect.TryParse, Expect.Format)

        // TODO: typed From

        let From_ =
            Header_ "From"

        let Host_ =
            value_
                "Host"
                (Host.TryParse, Host.Format)

        let IfMatch_ =
            value_
                "If-Match"
                (IfMatch.TryParse, IfMatch.Format)

        let IfModifiedSince_ =
            value_
                "If-Modified-Since"
                (IfModifiedSince.TryParse, IfModifiedSince.Format)

        let IfNoneMatch_ =
            value_
                "If-None-Match"
                (IfNoneMatch.TryParse, IfNoneMatch.Format)

        let IfRange_ =
            value_
                "If-Range"
                (IfRange.TryParse, IfRange.Format)

        let IfUnmodifiedSince_ =
            value_
                "If-Unmodified-Since"
                (IfUnmodifiedSince.TryParse, IfUnmodifiedSince.Format)

        let MaxForwards_ =
            value_
                "Max-Forwards"
                (MaxForwards.TryParse, MaxForwards.Format)

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
            value_
                "Referer"
                (Referer.TryParse, Referer.Format)

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
            Environment.value_<Stream> Constants.responseBody
       <--> Option.unsafe_

    let Headers_ =
            Environment.value_<IDictionary<string, string []>> Constants.responseHeaders
       <--> Option.unsafe_

    let Header_ key =
            Headers_
       >--> Dict.value_<string, string []> key
       <--> Option.mapIsomorphism ((String.concat ","), (Array.create 1))

    let HttpVersion_ =
            Environment.value_<string> Constants.responseProtocol
       <--> Option.mapIsomorphism (HttpVersion.Parse, HttpVersion.Format)

    let ReasonPhrase_ =
            Environment.value_<string> Constants.responseReasonPhrase

    let StatusCode_ =
            Environment.value_<int> Constants.responseStatusCode

    (* Response Header Lenses *)

    [<RequireQualifiedAccess>]
    module Headers =

        let private value_ key e =
                Header_ key
           <--> Option.mapEpimorphism e

        // TODO: typed AcceptRanges

        let AcceptRanges_ =
            Header_ "Accept-Ranges"

        let Age_ =
            value_
                "Age"
                (Age.TryParse, Age.Format)

        let Allow_ =
            value_
                "Allow"
                (Allow.TryParse, Allow.Format)

        let CacheControl_ =
            value_
                "Cache-Control"
                (CacheControl.TryParse, CacheControl.Format)

        let Connection_ =
            value_
                "Connection"
                (Connection.TryParse, Connection.Format)

        let ContentEncoding_ =
            value_
                "Content-Encoding"
                (ContentEncoding.TryParse, ContentEncoding.Format)

        let ContentLanguage_ =
            value_
                "Content-Language"
                (ContentLanguage.TryParse, ContentLanguage.Format)

        let ContentLength_ =
            value_
                "Content-Length"
                (ContentLength.TryParse, ContentLength.Format)

        let ContentLocation_ =
            value_
                "Content-Location"
                (ContentLocation.TryParse, ContentLocation.Format)

        // TODO: typed ContentRange

        let ContentRange_ =
            Header_ "Content-Range"

        let ContentType_ =
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

        let ProxyAuthenticate_ =
            Header_ "Proxy-Authenticate"

        // TODO: typed RetryAfter

        let RetryAfter_ =
            value_
                "Retry-After"
                (RetryAfter.TryParse, RetryAfter.Format)

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