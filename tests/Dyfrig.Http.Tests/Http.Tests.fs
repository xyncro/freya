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
                "Accept", [| "text/plain; q=0.5, text/html, text/x-dvi; q=0.8, text/x-c" |] ]
    
        let data = 
            dict [
                Constants.requestHeaders, box requestHeaders
                Constants.requestMethod, box "GET"
                Constants.requestPath, box "/some/path"
                Constants.requestPathBase, box ""
                Constants.requestProtocol, box "HTTP/1.1"
                Constants.requestQueryString, box "foo=bar&baz=boz"
                Constants.requestScheme, box "http" ]
        
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

        [<Test>]
        let ``accept header contains correct values`` () =
            let accept = test (getPLM Request.Headers.accept)

            accept.IsSome =? true
            accept.Value.Length =? 4
