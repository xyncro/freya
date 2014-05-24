open Dyfrig
open Fuchu
open Swensen.Unquote

let initializingTests =
    let env =
        new Environment(
            requestMethod = "GET",
            requestScheme = "http",
            requestPathBase = "",
            requestPath = "/",
            requestQueryString = "",
            requestProtocol = "HTTP/1.1",
            requestHeaders = dict [| "Host", [|"example.org"|] |]
        )
    testList "Initializing a GET request for http://example.org/ using HTTP/1.1" [
        testCase "should set the RequestMethod to GET" <| fun _ ->
            test <@ env.RequestMethod = "GET" @>
        testCase "should set the owin.RequestMethod to GET" <| fun _ ->
            test <@ unbox env.[Constants.requestMethod] = "GET" @>
        testCase "should set the RequestScheme to http" <| fun _ ->
            test <@ env.RequestScheme = "http" @>
        testCase "should set the owin.RequestScheme to http" <| fun _ ->
            test <@ unbox env.[Constants.requestScheme] = "http" @>
        testCase "should set the RequestPathBase to \"\"" <| fun _ ->
            test <@ env.RequestPathBase = "" @>
        testCase "should set the owin.RequestPathBase to \"\"" <| fun _ ->
            test <@ unbox env.[Constants.requestPathBase] = "" @>
        testCase "should set the RequestPath to /" <| fun _ ->
            test <@ env.RequestPath = "/" @>
        testCase "should set the owin.RequestPath to /" <| fun _ ->
            test <@ unbox env.[Constants.requestPath] = "/" @>
        testCase "should set the RequestQueryString to \"\"" <| fun _ ->
            test <@ env.RequestQueryString = "" @>
        testCase "should set the owin.RequestQueryString to \"\"" <| fun _ ->
            test <@ unbox env.[Constants.requestQueryString] = "" @>
        testCase "should set the RequestProtocol to HTTP/1.1" <| fun _ ->
            test <@ env.RequestProtocol = "HTTP/1.1" @>
        testCase "should include the Host header in the request headers" <| fun _ ->
            test <@ env.RequestHeaders.["Host"].[0] = "example.org" @>
        testCase "should set the owin.RequestProtocol to HTTP/1.1" <| fun _ ->
            test <@ unbox env.[Constants.requestProtocol] = "HTTP/1.1" @>
        testCase "should set the RequestBody to Stream.Null" <| fun _ ->
            test <@ env.RequestBody = System.IO.Stream.Null @>
        testCase "should set the owin.RequestBody to Stream.Null" <| fun _ ->
            test <@ unbox env.[Constants.requestBody] = System.IO.Stream.Null @>
        testCase "should set the ResponseStatusCode to 200" <| fun _ ->
            test <@ env.ResponseStatusCode = 200 @>
        testCase "should set the owin.ResponseStatusCode to 200" <| fun _ ->
            raises<System.Collections.Generic.KeyNotFoundException> <@ env.[Constants.responseStatusCode] @>
        testCase "should set the ResponseReasonPhrase to OK" <| fun _ ->
            test <@ unbox env.ResponseReasonPhrase = "OK" @>
        testCase "should set the owin.ResponseReasonPhrase to OK" <| fun _ ->
            raises<System.Collections.Generic.KeyNotFoundException> <@ env.[Constants.responseReasonPhrase] @>
        testCase "should set the ResponseHeaders to an empty dictionary" <| fun _ ->
            test <@ env.ResponseHeaders.GetType() = typeof<System.Collections.Generic.Dictionary<string, string[]>> @>
        testCase "should set the owin.ResponseHeaders to an empty dictionary" <| fun _ ->
            test <@ env.[Constants.responseHeaders].GetType() = typeof<System.Collections.Generic.Dictionary<string, string[]>> @>
        testCase "should ensure the cached response headers value and the dictionary value are the same reference" <| fun _ ->
            test <@ obj.ReferenceEquals(env.ResponseHeaders, env.[Constants.responseHeaders]) @>
        testCase "should set the ResponseBody to a MemoryStream" <| fun _ ->
            test <@ env.ResponseBody.GetType() = typeof<System.IO.MemoryStream> @>
        testCase "should set the owin.ResponseBody to a MemoryStream" <| fun _ ->
            test <@ env.[Constants.responseBody].GetType() = typeof<System.IO.MemoryStream> @>
        testCase "should ensure the cached response body value and the dictionary value are the same reference" <| fun _ ->
            test <@ obj.ReferenceEquals(env.ResponseBody, env.[Constants.responseBody]) @>
        testCase "should set the RequestMethod to when owin.RequestMethod changes" <| fun _ ->
            env.[Constants.requestMethod] <- "POST"
            test <@ env.RequestMethod = unbox env.[Constants.requestMethod] @>
        testCase "should return http://example.org/ as from GetRequestUri()" <| fun _ ->
            test <@ env.GetRequestUri() = Some "http://example.org/" @>
    ]

let environmentModuleTests =
    let env =
        new Environment(
            requestMethod = "GET",
            requestScheme = "http",
            requestPathBase = "",
            requestPath = "/",
            requestQueryString = "",
            requestProtocol = "HTTP/1.1",
            requestHeaders = dict [| "Host", [|"example.org"|] |]
        )
    testList "When creating an Environment from an OWIN environment dictionary" [
        testCase "should return the original Environment if it is of type Environment" <| fun _ ->
            test <@ Environment.toEnvironment env = env @>
    ]

[<EntryPoint>]
let main argv =
    testList "Environment tests" [ initializingTests; environmentModuleTests ] |> runParallel
