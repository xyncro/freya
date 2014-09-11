namespace Dyfrig.Http.Tests

open System.Collections.Generic
open NUnit.Framework
open Swensen.Unquote
open Dyfrig.Core
open Dyfrig.Http


[<AutoOpen>]
module Data =

    let env () =
    
        let env = Dictionary<string, obj> () :> IDictionary<string, obj>

        // Request

        env.[Constants.requestMethod] <- "GET"
        env.[Constants.requestPath] <- "/some/path"
        env.[Constants.requestPathBase] <- ""
        env.[Constants.requestProtocol] <- "HTTP/1.1"
        env.[Constants.requestQueryString] <- "foo=bar&baz=boz"
        env.[Constants.requestScheme] <- "http"

        // Request Headers

        let requestHeaders = Dictionary<string, string []> () :> IDictionary<string, string []>

        requestHeaders.["Accept"] <- [| "text/plain; q=0.5, text/html, text/x-dvi; q=0.8, text/x-c" |]

        env.[Constants.requestHeaders] <- requestHeaders
        
        // Response

        // Response Headers

        let responseHeaders = Dictionary<string, string []> () :> IDictionary<string, string []> 

        env.[Constants.responseHeaders] <- responseHeaders
        
        env


[<AutoOpen>]
module Helpers =

    let test f =
        Async.RunSynchronously (f (env ())) |> fst


module Request =

    [<Test>]
    let ``method lens returns correct method value`` () =
        test (getLM Request.Method) =? Method.GET

    [<Test>]
    let ``path lens returns correct path value`` () =
        test (getLM Request.Path) =? "/some/path"

    [<Test>]
    let ``path base lens returns correct path base value`` () =
        test (getLM Request.PathBase) =? ""

    [<Test>]
    let ``protocol lens returns correct protocol value`` () =
        test (getLM Request.Protocol) =? Protocol.HTTP 1.1

    [<Test>]
    let ``query lens returns correct query value`` () =
        test (getPLM (Request.Query "foo")) =? Some "bar"
        test (getPLM (Request.Query "baz")) =? Some "boz"
        test (getPLM (Request.Query "qux")) =? None

    [<Test>]
    let ``scheme lens returns correct scheme value`` () =
        test (getLM Request.Scheme) =? Scheme.HTTP


    module Headers =

        [<Test>]
        let ``accept header contains correct values`` () =
            let accept = test (getPLM Request.Headers.Accept)

            accept.IsSome =? true
            accept.Value.Length =? 4