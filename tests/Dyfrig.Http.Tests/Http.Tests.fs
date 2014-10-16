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


[<AutoOpen>]
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


[<AutoOpen>]
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
        |> getPL (Request.headersKey k <?-> headerIso)


module Lenses =

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
    let ``Request.protocol`` () =
        let protocolTyped = Protocol.HTTP 1.1
        let protocolString = "HTTP/1.1"

        let get = 
            getT
                (Constants.requestProtocol, protocolString)
                (getLM Request.protocol)

        let set =
            setT
                (setLM Request.protocol protocolTyped)
                Constants.requestProtocol

        get =? protocolTyped
        set =? protocolString

    [<Test>]
    let ``Request.query`` () =
        let queryTyped =
            Map.ofList 
                [ "foo", "bar"
                  "baz", "boz" ]

        let queryString =
            "foo=bar&baz=boz"

        let get = 
            getT 
                (Constants.requestQueryString, queryString)
                (getLM Request.query)

        let set =
            setT
                (setLM Request.query queryTyped)
                Constants.requestQueryString

        get =? queryTyped
        set =? queryString

    [<Test>]
    let ``Request.scheme`` () =
        let schemeTyped = Scheme.HTTP
        let schemeString = "http"

        let get = 
            getT
                (Constants.requestScheme, schemeString)
                (getLM Request.scheme)

        let set =
            setT
                (setLM Request.scheme schemeTyped)
                Constants.requestScheme

        get =? schemeTyped
        set =? schemeString

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
        let dateTyped = DateTime.Parse ("1994/10/29 19:43:31")
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
        let dateTyped = DateTime.Parse ("1994/10/29 19:43:31")
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
            [ { MediaRange = MediaRange.Closed (ClosedMediaRange (MediaType "application", MediaSubType "json"))
                MediaRangeParameters = Map.empty
                ExtensionParameters = Map.empty
                Weight = Some 0.8 }
              { MediaRange = MediaRange.Partial (PartialMediaRange  (MediaType "text"))
                MediaRangeParameters = Map.empty
                ExtensionParameters = Map.empty
                Weight = Some 0.7 }
              { MediaRange = MediaRange.Open
                MediaRangeParameters = Map.empty
                ExtensionParameters = Map.empty
                Weight = Some 0.5 } ]

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
            [ { Charset = Charset.Specified (SpecifiedCharset.Named "iso-8859-5")
                Weight = None }
              { Charset = Charset.Specified (SpecifiedCharset.Named "unicode-1-1")
                Weight = Some 0.8 } ]

        let acceptCharsetString =
            "iso-8859-5,unicode-1-1;q=0.8"

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
            [ { Encoding = Encoding.Specified (SpecifiedEncoding.Named "gzip")
                Weight = None }
              { Encoding = Encoding.Specified (SpecifiedEncoding.Identity)
                Weight = Some 0.5 }
              { Encoding = Encoding.Any
                Weight = Some 0. } ]

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
                Weight = Some 0.7 } ]

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
        let maxForwardsTyped = 5
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


module Negotiation =

    [<Test>]
    let ``negotiateAccept`` =
        let available =
            [ ClosedMediaRange (MediaType "application", MediaSubType "json")
              ClosedMediaRange (MediaType "text", MediaSubType "html") ]

        let requested1 =
            [ { MediaRange = Closed (ClosedMediaRange (MediaType "application", MediaSubType "json"))
                MediaRangeParameters = Map.empty
                ExtensionParameters = Map.empty
                Weight = Some 0.8 }
              { MediaRange = Partial (PartialMediaRange (MediaType "text"))
                MediaRangeParameters = Map.empty
                ExtensionParameters = Map.empty
                Weight = Some 0.5 } ]

        let requested2 =
            [ { MediaRange = Closed (ClosedMediaRange (MediaType "application", MediaSubType "json"))
                MediaRangeParameters = Map.empty
                ExtensionParameters = Map.empty
                Weight = Some 0.8 }
              { MediaRange = Partial (PartialMediaRange (MediaType "text"))
                MediaRangeParameters = Map.empty
                ExtensionParameters = Map.empty
                Weight = Some 0.9 } ]

        let requested3 =
            [ { MediaRange = Open
                MediaRangeParameters = Map.empty
                ExtensionParameters = Map.empty
                Weight = Some 0. } ]

        let negotiated1 = negotiateAccept available requested1
        let negotiated2 = negotiateAccept available requested2
        let negotiated3 = negotiateAccept available requested3
        
        negotiated1 =? [ ClosedMediaRange (MediaType "application", MediaSubType "json")
                         ClosedMediaRange (MediaType "text", MediaSubType "html") ]
        negotiated2 =? [ ClosedMediaRange (MediaType "text", MediaSubType "html")
                         ClosedMediaRange (MediaType "application", MediaSubType "json") ]
        negotiated3 =? []

    [<Test>]
    let ``negotiateAcceptCharset`` () =
        let available =
            [ SpecifiedCharset.Named "unicode-1-1"
              SpecifiedCharset.Named "iso-8859-1" ]

        let requested1 =
            [ { Charset = Charset.Specified (SpecifiedCharset.Named "unicode-1-1")
                Weight = Some 0.8 }
              { Charset = Charset.Specified (SpecifiedCharset.Named "iso-8859-1")
                Weight = Some 0.9 } ]

        let requested2 =
            [ { Charset = Charset.Specified (SpecifiedCharset.Named "unicode-1-1")
                Weight = None }
              { Charset = Charset.Specified (SpecifiedCharset.Named "iso-8859-1")
                Weight = Some 0.9 } ]

        let negotiated1 = negotiateAcceptCharset available requested1
        let negotiated2 = negotiateAcceptCharset available requested2

        negotiated1 =? [ SpecifiedCharset.Named "iso-8859-1"
                         SpecifiedCharset.Named "unicode-1-1" ]
        negotiated2 =? [ SpecifiedCharset.Named "unicode-1-1"
                         SpecifiedCharset.Named "iso-8859-1" ]

    [<Test>]
    let ``negotiateAcceptEncoding`` () =
        let available =
            [ SpecifiedEncoding.Named "gzip" ]

        let requested1 =
            [ { Encoding = Encoding.Specified (SpecifiedEncoding.Named "gzip")
                Weight = Some 0.7 } ]

        let requested2 =
            [ { Encoding = Encoding.Specified (SpecifiedEncoding.Named "compress")
                Weight = Some 0.7 } ]

        let negotiated1 = negotiateAcceptEncoding available requested1
        let negotiated2 = negotiateAcceptEncoding available requested2

        negotiated1 =? [ SpecifiedEncoding.Named "gzip" ]
        negotiated2 =? []
