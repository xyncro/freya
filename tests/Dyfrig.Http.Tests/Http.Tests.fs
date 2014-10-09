namespace Dyfrig.Http.Tests

open System
open System.Collections.Generic
open System.Globalization
open NUnit.Framework
open Swensen.Unquote
open Aether
open Aether.Operators
open Dyfrig.Core
open Dyfrig.Http


module Read =

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


module Write =

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
        |> getPL (Request.header k <?-> headerIso)


module Lenses =

    open Read
    open Write

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
        getT (Constants.requestPath, "/some/path") (getLM Request.path) =? "/some/path"

    [<Test>]
    let ``Request.pathBase`` () =
        getT (Constants.requestPathBase, "") (getLM Request.pathBase) =? ""

    [<Test>]
    let ``Request.protocol`` () =
        getT (Constants.requestProtocol, "HTTP/1.0") (getLM Request.protocol) =? Protocol.HTTP 1.0
        getT (Constants.requestProtocol, "HTTP/1.1") (getLM Request.protocol) =? Protocol.HTTP 1.1
        getT (Constants.requestProtocol, "Other") (getLM Request.protocol) =? Protocol.Custom "Other"

    [<Test>]
    let ``Request.query`` () =
        let getT' = getT (Constants.requestQueryString, "foo=bar&baz=boz")

        getT' (getPLM (Request.query "foo")) =? Some "bar"
        getT' (getPLM (Request.query "baz")) =? Some "boz"
        getT' (getPLM (Request.query "qux")) =? None

    [<Test>]
    let ``Request.scheme`` () =
        getT (Constants.requestScheme, "http") (getLM Request.scheme) =? Scheme.HTTP

    [<Test>]
    let ``Request.Conditionals.ifMatch`` () =
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
                (getPLM Request.Conditionals.ifMatch)

        let set =
            setRequestHeaderT
                (setPLM Request.Conditionals.ifMatch ifMatchTyped)
                "If-Match"

        get.Value =? ifMatchTyped
        set.Value =? ifMatchString

    [<Test>]
    let ``Request.Conditionals.ifNoneMatch`` () =
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
                (getPLM Request.Conditionals.ifNoneMatch)

        let set =
            setRequestHeaderT
                (setPLM Request.Conditionals.ifNoneMatch ifNoneMatchTyped)
                "If-None-Match"

        get.Value =? ifNoneMatchTyped
        set.Value =? ifNoneMatchString

    [<Test>]
    let ``Request.Conditionals.ifModifiedSince`` () =
        let dateTyped = DateTime.Parse ("1994/10/29 19:43:31")
        let dateString = "Sat, 29 Oct 1994 19:43:31 GMT"

        let get =
            getRequestHeaderT
                ("If-Modified-Since", dateString)
                (getPLM Request.Conditionals.ifModifiedSince)

        let set =
            setRequestHeaderT
                (setPLM Request.Conditionals.ifModifiedSince dateTyped)
                "If-Modified-Since"

        get.Value =? dateTyped
        set.Value =? dateString

    [<Test>]
    let ``Request.Conditionals.ifUnmodifiedSince`` () =
        let dateTyped = DateTime.Parse ("1994/10/29 19:43:31")
        let dateString = "Sat, 29 Oct 1994 19:43:31 GMT"

        let get =
            getRequestHeaderT
                ("If-Unmodified-Since", dateString)
                (getPLM Request.Conditionals.ifUnmodifiedSince)

        let set =
            setRequestHeaderT
                (setPLM Request.Conditionals.ifUnmodifiedSince dateTyped)
                "If-Unmodified-Since"

        get.Value =? dateTyped
        set.Value =? dateString

    [<Test>]
    let ``Request.ContentNegotiation.accept`` () =
        let x = 
            getRequestHeaderT 
                ("Accept", "text/plain; q=0.5, text/html, text/x-dvi; q=0.8, text/x-c") 
                (getPLM Request.ContentNegotiation.accept)

        x.IsSome =? true
        x.Value.Length =? 4
        x.Value.[0].MediaType =? MediaRange.Closed (ClosedMediaRange (MediaType "text", MediaSubType "plain"))
        x.Value.[0].Weight =? Some 0.5

//        [<Test>]
//        let ``negotiateAccept returns correct negotiated type/subtype pair`` () =
//            let available =
//                [ ClosedMediaRange (MediaType "text", MediaSubType "html")
//                  ClosedMediaRange (MediaType "application", MediaSubType "json")
//                  ClosedMediaRange (MediaType "text", MediaSubType "plain") ]
//
//            let requested =
//                [ { MediaType = MediaRange.Closed (ClosedMediaRange (MediaType "application", MediaSubType "json"))
//                    MediaTypeParameters = Map.empty
//                    ExtensionParameters = Map.empty
//                    Weight = Some 0.8 }
//                  { MediaType = MediaRange.Closed (ClosedMediaRange  (MediaType "text", MediaSubType "html"))
//                    MediaTypeParameters = Map.empty
//                    ExtensionParameters = Map.empty
//                    Weight = Some 0.7 }
//                  { MediaType = MediaRange.Open
//                    MediaTypeParameters = Map.empty
//                    ExtensionParameters = Map.empty
//                    Weight = Some 0.5 } ]
//
//            negotiateAccept available requested =? Some (ClosedMediaRange (MediaType "application", MediaSubType "json"))

    [<Test>]
    let ``Request.ContentNegotiation.acceptCharset`` () =
        let acceptCharsetTyped =
            [ { Charset = Charset.Named (NamedCharset "iso-8859-5")
                Weight = None }
              { Charset = Charset.Named (NamedCharset "unicode-1-1")
                Weight = Some 0.8 } ]

        let acceptCharsetString =
            "iso-8859-5,unicode-1-1;q=0.8"

        let get = 
            getRequestHeaderT
                ("Accept-Charset", acceptCharsetString)
                (getPLM Request.ContentNegotiation.acceptCharset)

        let set =
            setRequestHeaderT
                (setPLM (Request.ContentNegotiation.acceptCharset) acceptCharsetTyped)
                "Accept-Charset"

        get.Value =? acceptCharsetTyped
        set.Value =? acceptCharsetString

    [<Test>]
    let ``Request.ContentNegotiation.acceptEncoding`` () =
        let acceptEncodingTyped =
            [ { Encoding = Encoding.Named (NamedEncoding "gzip")
                Weight = None }
              { Encoding = Encoding.Identity
                Weight = Some 0.5 }
              { Encoding = Encoding.Any
                Weight = Some 0. } ]

        let acceptEncodingString = 
            "gzip,identity;q=0.5,*;q=0"

        let get = 
            getRequestHeaderT
                ("Accept-Encoding", acceptEncodingString)
                (getPLM Request.ContentNegotiation.acceptEncoding)

        let set =
            setRequestHeaderT
                (setPLM (Request.ContentNegotiation.acceptEncoding) acceptEncodingTyped)
                "Accept-Encoding"

        get.Value =? acceptEncodingTyped
        set.Value =? acceptEncodingString

    [<Test>]
    let ``Request.ContentNegotiation.acceptLanguage`` () =
        let acceptLanguageTyped =
            [ { Language = CultureInfo ("da")
                Weight = None }
              { Language = CultureInfo ("en-gb")
                Weight = Some 0.8 }
              { Language = CultureInfo ("en")
                Weight = Some 0.7 } ]

        let acceptLanguageString = 
            "da,en-GB;q=0.8,en;q=0.7"

        let get = 
            getRequestHeaderT
                ("Accept-Language", acceptLanguageString)
                (getPLM Request.ContentNegotiation.acceptLanguage)

        let set =
            setRequestHeaderT
                (setPLM (Request.ContentNegotiation.acceptLanguage) acceptLanguageTyped)
                "Accept-Language"

        get.Value =? acceptLanguageTyped
        set.Value =? acceptLanguageString

    [<Test>]
    let ``Request.Controls.host`` () =
        let get = 
            getRequestHeaderT
                ("Host", "www.example.org:8080")
                (getPLM Request.Controls.host)

        get.Value =? "www.example.org:8080"

    [<Test>]
    let ``Request.Controls.maxForwards`` () =
        let maxForwardsTyped = 5
        let maxForwardsString = "5"

        let get = 
            getRequestHeaderT
                ("Max-Forwards", maxForwardsString)
                (getPLM Request.Controls.maxForwards)

        let set =
            setRequestHeaderT
                (setPLM (Request.Controls.maxForwards) maxForwardsTyped)
                "Max-Forwards"

        get.Value =? maxForwardsTyped
        set.Value =? maxForwardsString
