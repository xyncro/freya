namespace Dyfrig.Http.Tests

open System
open System.Collections.Generic
open NUnit.Framework
open Swensen.Unquote
open Dyfrig.Core
open Dyfrig.Http


[<AutoOpen>]
module Helpers =

    let test (k, v) f =
        let data = dict [ k, box v ]
        let env = Dictionary<string, obj> (data, StringComparer.OrdinalIgnoreCase)

        Async.RunSynchronously (f env) |> fst

    let testRequestHeader (k, h) f =
        let headers = dict [ k, [| h |] ]
        let data = dict [ Constants.requestHeaders, box headers ]
        let env = Dictionary<string, obj> (data, StringComparer.OrdinalIgnoreCase)

        Async.RunSynchronously (f env) |> fst


module Lenses =

    (* Request *)

    [<Test>]
    let ``Request.meth`` () =
        test (Constants.requestMethod, "GET") (getLM Request.meth) =? Method.GET

    [<Test>]
    let ``Request.path`` () =
        test (Constants.requestPath, "/some/path") (getLM Request.path) =? "/some/path"

    [<Test>]
    let ``Request.pathBase`` () =
        test (Constants.requestPathBase, "") (getLM Request.pathBase) =? ""

    [<Test>]
    let ``Request.protocol`` () =
        test (Constants.requestProtocol, "HTTP/1.0") (getLM Request.protocol) =? Protocol.HTTP 1.0
        test (Constants.requestProtocol, "HTTP/1.1") (getLM Request.protocol) =? Protocol.HTTP 1.1
        test (Constants.requestProtocol, "Other") (getLM Request.protocol) =? Protocol.Custom "Other"

    [<Test>]
    let ``Request.query`` () =
        let test' = test (Constants.requestQueryString, "foo=bar&baz=boz")

        test' (getPLM (Request.query "foo")) =? Some "bar"
        test' (getPLM (Request.query "baz")) =? Some "boz"
        test' (getPLM (Request.query "qux")) =? None

    [<Test>]
    let ``Request.scheme`` () =
        test (Constants.requestScheme, "http") (getLM Request.scheme) =? Scheme.HTTP

    (* Content Negotiation
            
       Taken from RFC 7231, Section 5.3
       [http://tools.ietf.org/html/rfc7231#section-5.3] *)

    (* Accept

       Taken from RFC 7231, Section 5.3.2. Accept
       [http://tools.ietf.org/html/rfc7231#section-5.3.2] *)

    [<Test>]
    let ``Request.ContentNegotiation.accept`` () =
        let x = 
            testRequestHeader 
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

    (* Accept-Charset

       Taken from RFC 7231, Section 5.3.3. Accept-Charset
       [http://tools.ietf.org/html/rfc7231#section-5.3.3] *)

    [<Test>]
    let ``Request.ContentNegotiation.acceptCharset`` () =
        let x = 
            testRequestHeader
                ("Accept-Charset", "iso-8859-5, unicode-1-1;q=0.8")
                (getPLM Request.ContentNegotiation.acceptCharset)

        x.IsSome =? true
        x.Value.Length =? 2
        x.Value.[0].Charset =? Charset.Named (NamedCharset "iso-8859-5")
        x.Value.[0].Weight =? None
        x.Value.[1].Charset =? Charset.Named (NamedCharset "unicode-1-1")
        x.Value.[1].Weight =? Some 0.8

    (* Accept-Encoding

       Taken from RFC 7231, Section 5.3.4. Accept-Encoding
       [http://tools.ietf.org/html/rfc7231#section-5.3.4] *)

    [<Test>]
    let ``Request.ContentNegotiation.acceptEncoding`` () =
        let x = 
            testRequestHeader
                ("Accept-Encoding", "gzip;q=1.0, identity; q=0.5, *;q=0")
                (getPLM Request.ContentNegotiation.acceptEncoding)

        x.IsSome =? true
        x.Value.Length =? 3
        x.Value.[0].Encoding =? Encoding.Named (NamedEncoding "gzip")
        x.Value.[0].Weight =? Some 1.
        x.Value.[1].Encoding =? Encoding.Identity
        x.Value.[1].Weight =? Some 0.5
        x.Value.[2].Encoding =? Encoding.Any
        x.Value.[2].Weight =? Some 0.

    (* Accept-Language

       Taken from RFC 7231, Section 5.3.5. Accept-Language
       [http://tools.ietf.org/html/rfc7231#section-5.3.5] *)

    [<Test>]
    let ``Request.ContentNegotiation.acceptLanguage`` () =
        let x = 
            testRequestHeader
                ("Accept-Language", "da, en-gb;q=0.8, en;q=0.7")
                (getPLM Request.ContentNegotiation.acceptLanguage)

        x.IsSome =? true
        x.Value.Length =? 3
        x.Value.[0].Language.Name =? "da"
        x.Value.[0].Weight =? None
        x.Value.[1].Language.Name =? "en-GB"
        x.Value.[1].Weight =? Some 0.8
        x.Value.[2].Language.Name =? "en"
        x.Value.[2].Weight =? Some 0.7

    (* Conditionals
            
        Taken from RFC 7232, Section 3
        [http://tools.ietf.org/html/rfc7232#section-3] *)

    (* If-Match

       Taken from RFC 7232, Section 3.1, If-Match
       [http://tools.ietf.org/html/rfc7232#section-3.1] *)

    [<Test>]
    let ``Request.Conditionals.ifMatch`` () =
        let x = 
            testRequestHeader
                ("If-Match", "\"xyzzy\", \"r2d2xxxx\", \"c3piozzzz\"")
                (getPLM Request.Conditionals.ifMatch)

        let y = x |> function | Some (IfMatch.EntityTags x) -> Some x | _ -> None

        y.IsSome =? true

        y.Value.Length =? 3
        y.Value.[0] =? EntityTag.Strong "xyzzy"

    (* If-None-Match

       Taken from RFC 7232, Section 3.2, If-None-Match
       [http://tools.ietf.org/html/rfc7232#section-3.2] *)

    [<Test>]
    let ``Request.Conditionals.ifNoneMatch`` () =
        let x = 
            testRequestHeader
                ("If-None-Match", "\"xyzzy\", \"r2d2xxxx\", \"c3piozzzz\"")
                (getPLM Request.Conditionals.ifNoneMatch)

        let y = x |> function | Some (IfNoneMatch.EntityTags x) -> Some x | _ -> None

        y.IsSome =? true
        y.Value.Length =? 3
        y.Value.[0] =? EntityTag.Strong "xyzzy"

    (* If-Modified-Since

       Taken from RFC 7232, Section 3.3, If-Modified-Since
       [http://tools.ietf.org/html/rfc7232#section-3.3] *)

    [<Test>]
    let ``Request.Conditionals.ifModifiedSince`` () =
        let x =
            testRequestHeader
                ("If-Modified-Since", "Sat, 29 Oct 1994 19:43:31 GMT")
                (getPLM Request.Conditionals.ifModifiedSince)

        x.IsSome =? true
        x.Value.Year =? 1994
        x.Value.Month =? 10
        x.Value.Day =? 29
        x.Value.Hour =? 19
        x.Value.Minute =? 43
        x.Value.Second =? 31
        x.Value.Kind =? DateTimeKind.Utc

    (* If-Unmodified-Since

       Taken from RFC 7232, Section 3.4, If-Unmodified-Since
       [http://tools.ietf.org/html/rfc7232#section-3.4] *)

    [<Test>]
    let ``Request.Conditionals.ifUnmodifiedSince`` () =
        let x =
            testRequestHeader
                ("If-Unmodified-Since", "Sat, 29 Oct 1994 19:43:31 GMT")
                (getPLM Request.Conditionals.ifUnmodifiedSince)

        x.IsSome =? true
        x.Value.Year =? 1994
        x.Value.Month =? 10
        x.Value.Day =? 29
        x.Value.Hour =? 19
        x.Value.Minute =? 43
        x.Value.Second =? 31
        x.Value.Kind =? DateTimeKind.Utc