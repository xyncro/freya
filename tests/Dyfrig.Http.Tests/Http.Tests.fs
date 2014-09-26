namespace Dyfrig.Http.Tests

open System
open System.Collections.Generic
open NUnit.Framework
open Swensen.Unquote
open Dyfrig.Core
open Dyfrig.Http


[<AutoOpen>]
module Data =

    let env () =

        let requestHeaders = 
            dict [
                "Accept",          [| "text/plain; q=0.5, text/html, text/x-dvi; q=0.8, text/x-c" |]
                "Accept-Charset",  [| "iso-8859-5, unicode-1-1;q=0.8" |]
                "Accept-Encoding", [| "gzip;q=1.0, identity; q=0.5, *;q=0" |]
                "Accept-Language", [| "da, en-gb;q=0.8, en;q=0.7" |]
                "If-Match",        [| "\"xyzzy\", \"r2d2xxxx\", \"c3piozzzz\"" |]
                "If-None-Match",   [| "\"xyzzy\", \"r2d2xxxx\", \"c3piozzzz\"" |] ]
    
        let data = 
            dict [
                Constants.requestHeaders,     box requestHeaders
                Constants.requestMethod,      box "GET"
                Constants.requestPath,        box "/some/path"
                Constants.requestPathBase,    box ""
                Constants.requestProtocol,    box "HTTP/1.1"
                Constants.requestQueryString, box "foo=bar&baz=boz"
                Constants.requestScheme,      box "http" ]
        
        Dictionary<string, obj> (data, StringComparer.OrdinalIgnoreCase)


[<AutoOpen>]
module Helpers =

    let test f =
        Async.RunSynchronously (f (env ())) |> fst


module Request =

    [<Test>]
    let ``method lens returns correct method value`` () =
        test (getLM Request.meth) =? Method.GET

    [<Test>]
    let ``path lens returns correct path value`` () =
        test (getLM Request.path) =? "/some/path"

    [<Test>]
    let ``path base lens returns correct path base value`` () =
        test (getLM Request.pathBase) =? ""

    [<Test>]
    let ``protocol lens returns correct protocol value`` () =
        test (getLM Request.protocol) =? Protocol.HTTP 1.1

    [<Test>]
    let ``query lens returns correct query value`` () =
        test (getPLM (Request.query "foo")) =? Some "bar"
        test (getPLM (Request.query "baz")) =? Some "boz"
        test (getPLM (Request.query "qux")) =? None

    [<Test>]
    let ``scheme lens returns correct scheme value`` () =
        test (getLM Request.scheme) =? Scheme.HTTP


    module Headers =

        // Content-Negotiation

        [<Test>]
        let ``accept header contains correct values`` () =
            let x = test (getPLM Request.Headers.accept)

            x.IsSome =? true
            x.Value.Length =? 4

            x.Value.[0].MediaRange.Type =? MediaType.Named "text"
            x.Value.[0].MediaRange.SubType =? MediaType.Named "plain"
            x.Value.[0].AcceptParameters.IsSome =? true
            x.Value.[0].AcceptParameters.Value.Weight =? 0.5

        [<Test>]
        let ``accept-charset header contains correct values`` () =
            let x = test (getPLM Request.Headers.acceptCharset)

            x.IsSome =? true
            x.Value.Length =? 2

            x.Value.[0].Charset =? Charset.Named "iso-8859-5"
            x.Value.[0].Weight =? None

            x.Value.[1].Charset =? Charset.Named "unicode-1-1"
            x.Value.[1].Weight =? Some 0.8

        [<Test>]
        let ``accept-encoding header contains correct values`` () =
            let x = test (getPLM Request.Headers.acceptEncoding)

            x.IsSome =? true
            x.Value.Length =? 3

            x.Value.[0].Encoding =? Encoding.Named "gzip"
            x.Value.[0].Weight =? Some 1.

            x.Value.[1].Encoding =? Encoding.Identity
            x.Value.[1].Weight =? Some 0.5

            x.Value.[2].Encoding =? Encoding.Any
            x.Value.[2].Weight =? Some 0.

        [<Test>]
        let ``accept-language header contains correct values`` () =
            let x = test (getPLM Request.Headers.acceptLanguage)

            x.IsSome =? true
            x.Value.Length =? 3

            x.Value.[0].Language.Name =? "da"
            x.Value.[0].Weight =? None

            x.Value.[1].Language.Name =? "en-GB"
            x.Value.[1].Weight =? Some 0.8

            x.Value.[2].Language.Name =? "en"
            x.Value.[2].Weight =? Some 0.7

        // Conditionals

        [<Test>]
        let ``if-match header contains correct values`` () =
            let x = test (getPLM Request.Headers.ifMatch)
            let y = x |> function | Some (IfMatch.EntityTags x) -> Some x | _ -> None

            y.IsSome =? true

            y.Value.Length =? 3
            y.Value.[0] =? "xyzzy"

        [<Test>]
        let ``if-none-match header contains correct values`` () =
            let x = test (getPLM Request.Headers.ifNoneMatch)
            let y = x |> function | Some (IfNoneMatch.EntityTags x) -> Some x | _ -> None

            y.IsSome =? true

            y.Value.Length =? 3
            y.Value.[0] =? "xyzzy"
            

