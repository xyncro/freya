module Freya.Typed.Tests.Lenses

open System
open System.Collections.Generic
open System.Globalization
open NUnit.Framework
open Swensen.Unquote
open Aether
open Aether.Operators
open Freya.Core
open Freya.Typed

// Isomorphisms

let boxIso<'a> : Iso<obj, 'a> =
    unbox<'a>, box

// Get

let getT (k, v) f =
    let data = dict [ k, box v ]
    let env = Dictionary<string, obj> (data, StringComparer.OrdinalIgnoreCase)

    Async.RunSynchronously (f env) 
    |> fst

let getRequestHeaderT (k, h) f =
    let headers = dict [ k, [| h |] ]
    let data = dict [ Constants.requestHeaders, box headers ]
    let env = Dictionary<string, obj> (data, StringComparer.OrdinalIgnoreCase)

    Async.RunSynchronously (f env) 
    |> fst

let getResponseHeaderT (k, h) f =
    let headers = dict [ k, [| h |] ]
    let data = dict [ Constants.responseHeaders, box headers ]
    let env = Dictionary<string, obj> (data, StringComparer.OrdinalIgnoreCase)

    Async.RunSynchronously (f env) 
    |> fst

// Set

let setT f k =
    let env = Dictionary<string, obj> (StringComparer.OrdinalIgnoreCase)

    Async.RunSynchronously (f env) 
    |> snd 
    |> getL ((dictLens k) <--> boxIso<string>)

let setRequestHeaderT f k =
    let headers = Dictionary<string, string []> (StringComparer.OrdinalIgnoreCase)
    let data = dict [ Constants.requestHeaders, box headers ]
    let env = Dictionary<string, obj> (data, StringComparer.OrdinalIgnoreCase)

    Async.RunSynchronously (f env) 
    |> snd 
    |> getPL (Request.headersKey k)

let setResponseHeaderT f k =
    let headers = Dictionary<string, string []> (StringComparer.OrdinalIgnoreCase)
    let data = dict [ Constants.responseHeaders, box headers ]
    let env = Dictionary<string, obj> (data, StringComparer.OrdinalIgnoreCase)

    Async.RunSynchronously (f env) 
    |> snd 
    |> getPL (Response.headersKey k)

// Tests

[<Test>]
let ``Request.meth`` () =
    let methTyped = GET
    let methString = "GET"

    let get =
        getT 
            (Constants.requestMethod, methString) 
            (getLM Request.meth)

    let set =
        setT
            (setLM Request.meth methTyped)
            Constants.requestMethod

    get =? methTyped
    set =? methString

[<Test>]
let ``Request.path`` () =
    let path = "/some/path"

    let get =
        getT 
            (Constants.requestPath, path) 
            (getLM Request.path)

    let set =
        setT
            (setLM Request.path path)
            Constants.requestPath

    get =? path
    set =? path

[<Test>]
let ``Request.pathBase`` () =
    let pathBase = "/home"

    let get =
        getT 
            (Constants.requestPathBase, pathBase) 
            (getLM Request.pathBase)

    let set =
        setT
            (setLM Request.pathBase pathBase)
            Constants.requestPathBase

    get =? pathBase
    set =? pathBase

[<Test>]
let ``Request.httpVersion`` () =
    let httpVersionTyped = HttpVersion.HTTP 1.1
    let httpVersionString = "HTTP/1.1"

    let get = 
        getT
            (Constants.requestProtocol, httpVersionString)
            (getLM Request.httpVersion)

    let set =
        setT
            (setLM Request.httpVersion httpVersionTyped)
            Constants.requestProtocol

    get =? httpVersionTyped
    set =? httpVersionString

// TODO: Reinstate

//[<Test>]
//let ``Request.query`` () =
//    let queryTyped =
//        Map.ofList 
//            [ "foo", "bar"
//              "baz", "boz" ]
//
//    let queryString =
//        "foo=bar&baz=boz"
//
//    let get = 
//        getT 
//            (Constants.requestQueryString, queryString)
//            (getLM Request.query)
//
//    let set =
//        setT
//            (setLM Request.query queryTyped)
//            Constants.requestQueryString
//
//    get =? queryTyped
//    set =? queryString

//[<Test>]
//let ``Request.scheme`` () =
//    let schemeTyped = Scheme.HTTP
//    let schemeString = "http"
//
//    let get = 
//        getT
//            (Constants.requestScheme, schemeString)
//            (getLM Request.scheme)
//
//    let set =
//        setT
//            (setLM Request.scheme schemeTyped)
//            Constants.requestScheme
//
//    get =? schemeTyped
//    set =? schemeString

[<Test>]
let ``Request.Headers.ifMatch`` () =
    let ifMatchTyped =
        IfMatch.EntityTags 
            [ Strong "xyzzy"
              Strong "r2d2xxxx"
              Strong "c3piozzzz" ]

    let ifMatchString =
        "\"xyzzy\",\"r2d2xxxx\",\"c3piozzzz\""

    let get = 
        getRequestHeaderT
            ("If-Match", ifMatchString)
            (getPLM Request.Headers.ifMatch)

    let set =
        setRequestHeaderT
            (setPLM Request.Headers.ifMatch ifMatchTyped)
            "If-Match"

    get.Value =? ifMatchTyped
    set.Value =? ifMatchString

[<Test>]
let ``Request.Headers.ifNoneMatch`` () =
    let ifNoneMatchTyped =
        IfNoneMatch.EntityTags 
            [ Strong "xyzzy"
              Strong "r2d2xxxx"
              Strong "c3piozzzz" ]

    let ifNoneMatchString =
        "\"xyzzy\",\"r2d2xxxx\",\"c3piozzzz\""

    let get = 
        getRequestHeaderT
            ("If-None-Match", ifNoneMatchString)
            (getPLM Request.Headers.ifNoneMatch)

    let set =
        setRequestHeaderT
            (setPLM Request.Headers.ifNoneMatch ifNoneMatchTyped)
            "If-None-Match"

    get.Value =? ifNoneMatchTyped
    set.Value =? ifNoneMatchString

[<Test>]
let ``Request.Headers.ifModifiedSince`` () =
    let dateTyped = IfModifiedSince (DateTime.Parse ("1994/10/29 19:43:31"))
    let dateString = "Sat, 29 Oct 1994 19:43:31 GMT"

    let get =
        getRequestHeaderT
            ("If-Modified-Since", dateString)
            (getPLM Request.Headers.ifModifiedSince)

    let set =
        setRequestHeaderT
            (setPLM Request.Headers.ifModifiedSince dateTyped)
            "If-Modified-Since"

    get.Value =? dateTyped
    set.Value =? dateString

[<Test>]
let ``Request.Headers.ifUnmodifiedSince`` () =
    let dateTyped = IfUnmodifiedSince (DateTime.Parse ("1994/10/29 19:43:31"))
    let dateString = "Sat, 29 Oct 1994 19:43:31 GMT"

    let get =
        getRequestHeaderT
            ("If-Unmodified-Since", dateString)
            (getPLM Request.Headers.ifUnmodifiedSince)

    let set =
        setRequestHeaderT
            (setPLM Request.Headers.ifUnmodifiedSince dateTyped)
            "If-Unmodified-Since"

    get.Value =? dateTyped
    set.Value =? dateString

[<Test>]
let ``Request.Headers.accept`` () =
    let acceptTyped =
        [ { MediaRange = MediaRange.Closed (Type "application", SubType "json", Map.empty)
            Parameters =
                Some { Weight = 0.8
                       Extensions = Map.empty } }
          { MediaRange = MediaRange.Partial (Type "text", Map.empty)
            Parameters =
                Some { Weight = 0.7
                       Extensions = Map.empty } }
          { MediaRange = MediaRange.Open Map.empty
            Parameters =
                Some { Weight = 0.5
                       Extensions = Map.empty } } ] |> Accept

    let acceptString =
        "application/json;q=0.8,text/*;q=0.7,*/*;q=0.5"

    let get = 
        getRequestHeaderT 
            ("Accept", acceptString) 
            (getPLM Request.Headers.accept)

    let set =
        setRequestHeaderT
            (setPLM Request.Headers.accept acceptTyped)
            "Accept"

    get.Value =? acceptTyped
    set.Value =? acceptString

[<Test>]
let ``Request.Headers.acceptCharset`` () =
    let acceptCharsetTyped =
        [ { Charset = CharsetSpec.Charset (Charsets.Iso88591)
            Weight = None }
          { Charset = CharsetSpec.Charset (Charsets.Unicode)
            Weight = Some 0.8 } ] |> AcceptCharset

    let acceptCharsetString =
        "iso-8859-1,unicode-1-1;q=0.8"

    let get = 
        getRequestHeaderT
            ("Accept-Charset", acceptCharsetString)
            (getPLM Request.Headers.acceptCharset)

    let set =
        setRequestHeaderT
            (setPLM (Request.Headers.acceptCharset) acceptCharsetTyped)
            "Accept-Charset"

    get.Value =? acceptCharsetTyped
    set.Value =? acceptCharsetString

[<Test>]
let ``Request.Headers.acceptEncoding`` () =
    let acceptEncodingTyped =
        [ { Encoding = EncodingSpec.Encoding (Encodings.GZip)
            Weight = None }
          { Encoding = EncodingSpec.Identity
            Weight = Some 0.5 }
          { Encoding = EncodingSpec.Any
            Weight = Some 0. } ] |> AcceptEncoding

    let acceptEncodingString = 
        "gzip,identity;q=0.5,*;q=0"

    let get = 
        getRequestHeaderT
            ("Accept-Encoding", acceptEncodingString)
            (getPLM Request.Headers.acceptEncoding)

    let set =
        setRequestHeaderT
            (setPLM (Request.Headers.acceptEncoding) acceptEncodingTyped)
            "Accept-Encoding"

    get.Value =? acceptEncodingTyped
    set.Value =? acceptEncodingString

[<Test>]
let ``Request.Headers.acceptLanguage`` () =
    let acceptLanguageTyped =
        [ { Language = CultureInfo ("da")
            Weight = None }
          { Language = CultureInfo ("en-gb")
            Weight = Some 0.8 }
          { Language = CultureInfo ("en")
            Weight = Some 0.7 } ] |> AcceptLanguage

    let acceptLanguageString = 
        "da,en-GB;q=0.8,en;q=0.7"

    let get = 
        getRequestHeaderT
            ("Accept-Language", acceptLanguageString)
            (getPLM Request.Headers.acceptLanguage)

    let set =
        setRequestHeaderT
            (setPLM (Request.Headers.acceptLanguage) acceptLanguageTyped)
            "Accept-Language"

    get.Value =? acceptLanguageTyped
    set.Value =? acceptLanguageString

[<Test>]
let ``Request.Headers.host`` () =
    let get = 
        getRequestHeaderT
            ("Host", "www.example.org:8080")
            (getPLM Request.Headers.host)

    get.Value =? "www.example.org:8080"

[<Test>]
let ``Request.Headers.maxForwards`` () =
    let maxForwardsTyped = MaxForwards 5
    let maxForwardsString = "5"

    let get = 
        getRequestHeaderT
            ("Max-Forwards", maxForwardsString)
            (getPLM Request.Headers.maxForwards)

    let set =
        setRequestHeaderT
            (setPLM (Request.Headers.maxForwards) maxForwardsTyped)
            "Max-Forwards"

    get.Value =? maxForwardsTyped
    set.Value =? maxForwardsString

[<Test>]
let ``Response.Headers.retryAfter`` () =
    let retryAfterTyped1 = RetryAfter.Date (DateTime.Parse ("1994/10/29 19:43:31"))
    let retryAfterTyped2 = RetryAfter.Delay 30
    let retryAfterString1 = "Sat, 29 Oct 1994 19:43:31 GMT"
    let retryAfterString2 = "30"

    let get1 = 
        getResponseHeaderT
            ("Retry-After", retryAfterString1)
            (getPLM Response.Headers.retryAfter)

    let get2 = 
        getResponseHeaderT
            ("Retry-After", retryAfterString2)
            (getPLM Response.Headers.retryAfter)

    let set1 =
        setResponseHeaderT
            (setPLM (Response.Headers.retryAfter) retryAfterTyped1)
            "Retry-After"

    let set2 =
        setResponseHeaderT
            (setPLM (Response.Headers.retryAfter) retryAfterTyped2)
            "Retry-After"

    get1.Value =? retryAfterTyped1
    set1.Value =? retryAfterString1

    get2.Value =? retryAfterTyped2
    set2.Value =? retryAfterString2