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
    parse RFC7231.meth, formatMethod

let private protocolIso =
    RFC7231.parseProtocol, formatProtocol

let private schemeIso =
    RFC7231.parseScheme, formatScheme

let private queryIso =
    RFC7231.parseQuery, formatQuery

let private acceptPIso =
    parseP RFC7231.accept, formatAccept

let private acceptCharsetPIso =
    parseP RFC7231.acceptCharset, formatAcceptCharset

let private acceptEncodingPIso =
    parseP RFC7231.acceptEncoding, formatAcceptEncoding

let private acceptLanguagePIso =
    parseP RFC7231.acceptLanguage, formatAcceptLanguage

(* RFC 7232 *)
        
let private eTagPIso =
    parseP RFC7232.eTag, formatETag

let private ifMatchPIso =
    parseP RFC7232.ifMatch, formatIfMatch

let private ifNoneMatchPIso =
    parseP RFC7232.ifNoneMatch, formatIfNoneMatch

(* Lenses *)

let dictLens k : Lens<IDictionary<'k,'v>, 'v> =
    (fun d -> d.[k]),
    (fun v d -> d.[k] <- v; d)

let dictPLens k : PLens<IDictionary<'k,'v>, 'v> =
    (fun d -> d.TryGetValue k |> function | true, v -> Some v | _ -> None),
    (fun v d -> d.[k] <- v; d)   

let private item<'a> key =
    dictLens key <--> boxIso<'a>

let private pItem<'a> key =
    dictPLens key <?-> boxIso<'a>


[<RequireQualifiedAccess>]
module Request =

    let body =
        item<Stream> Constants.requestBody

    let headers =
        item<IDictionary<string, string []>> Constants.requestHeaders

    let headersKey key =
        headers >-?> dictPLens key

    let meth = 
        item<string> Constants.requestMethod <--> methodIso

    let path = 
        item<string> Constants.requestPath

    let pathBase =
        item<string> Constants.requestPathBase

    let protocol =
        item<string> Constants.requestProtocol <--> protocolIso

    let scheme = 
        item<string> Constants.requestScheme <--> schemeIso

    let query =
        item<string> Constants.requestQueryString <--> queryIso

    let queryKey key =
        query >-?> mapPLens key


    [<RequireQualifiedAccess>]
    module Headers =

        let private header key =
            headersKey key <?-> headerIso

        let accept =
            header "Accept" <??> acceptPIso

        let acceptCharset =
            header "Accept-Charset" <??> acceptCharsetPIso

        let acceptEncoding =
            header "Accept-Encoding" <??> acceptEncodingPIso

        let acceptLanguage =
            header "Accept-Language" <??> acceptLanguagePIso

        // TODO: typed Authorization

        let authorization =
            header "Authorization"

        // TODO: typed CacheControl

        let cacheControl =
            header "Cache-Control"

        // TODO: typed Connection

        let connection =
            header "Connection"

        // TODO: typed ContentEncoding

        let contentEncoding =
            header "Content-Encoding"

        // TODO: typed ContentLanguage

        let contentLanguage =
            header "Content-Language"

        let contentLength =
            header"Content-Length" <??> intPIso

        // TODO: typed ContentLocation

        let contentLocation =
            header "Content-Location"

        // TODO: typed ContentMD5

        let contentMD5 =
            header "Content-MD5"

        // TODO: typed ContentType

        let contentType =
            header "Content-Type"

        let date =
            header "Date" <??> dateTimePIso

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
            header"If-Match" <??> ifMatchPIso

        let ifModifiedSince =
            header "If-Modified-Since" <??> dateTimePIso

        let ifNoneMatch =
            header "If-None-Match" <??> ifNoneMatchPIso

        // TODO: typed IfRange

        let ifRange =
            header "If-Range"

        let ifUnmodifiedSince =
            header "If-Unmodified-Since" <??> dateTimePIso

        let maxForwards =
            header "Max-Forwards" <??> intPIso

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
        item<Stream> Constants.responseBody

    let headers =
        item<IDictionary<string, string []>> Constants.responseHeaders

    let headersKey key =
        headers >-?> dictPLens key

    let protocol =
        pItem<string> Constants.responseProtocol <?-> protocolIso

    let reasonPhrase =
        pItem<string> Constants.responseReasonPhrase

    let statusCode =
        pItem<int> Constants.responseStatusCode


    [<RequireQualifiedAccess>]
    module Headers =

        let header key =
            headersKey key <?-> headerIso

        // TODO: typed AcceptRanges

        let acceptRanges =
            header "Accept-Ranges"

        let age =
            header "Age" <??> intPIso

        // TODO: typed Allow

        let allow =
            header "Allow"

        // TODO: typed CacheControl

        let cacheControl =
            header "Cache-Control"

        // TODO: typed Connection

        let connection =
            header "Connection"

        // TODO: typed ContentEncoding

        let contentEncoding =
            header "Content-Encoding"

        // TODO: typed ContentLanguage

        let contentLanguage =
            header "Content-Language"

        let contentLength =
            header "Content-Length" <??> intPIso

        // TODO: typed ContentLocation

        let contentLocation =
            header "Content-Location"

        // TODO: typed ContentMD5

        let contentMD5 =
            header "Content-MD5"

        // TODO: typed ContentRange

        let contentRange =
            header "Content-Range"

        // TODO: typed ContentType

        let contentType =
            header "Content-Type"

        let date =
            header "Date" <??> dateTimePIso

        let eTag =
            header "ETag" <??> eTagPIso

        let expires =
            header "Expires" <??> dateTimePIso

        let lastModified =
            header "Last-Modified" <??> eTagPIso

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
        modL l f |> modM
