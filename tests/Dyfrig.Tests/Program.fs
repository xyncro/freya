open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Net.Http
open Dyfrig
open Fuchu
open Swensen.Unquote

let env() =
    new Environment(
        requestMethod = "GET",
        requestScheme = "http",
        requestPathBase = "",
        requestPath = "/",
        requestQueryString = "",
        requestProtocol = "HTTP/1.1",
        requestHeaders = dict [| "Host", [|"example.org"|] |]
    )

let initializingTests =
    let env = env()
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
        testCase "should return http://example.org/ as from GetBaseUri()" <| fun _ ->
            test <@ env.GetBaseUri() = Some "http://example.org/" @>
        testCase "should return http://example.org/ as from GetRequestUri()" <| fun _ ->
            test <@ env.GetRequestUri() = Some "http://example.org/" @>
    ]

let environmentModuleTests =
    testList "When creating an Environment from an OWIN environment dictionary" [
        testCase "should return the original Environment if it is of type Environment" <| fun _ ->
            let env = env()
            Environment.toEnvironment env =? env
        testCase "should return new instance when using immutable update With member" <| fun _ ->
            let env = env()
            let env' = env.With("test", "value")
            test <@ not (obj.ReferenceEquals(env, env')) @>
            test <@ not (env.ContainsKey("test")) @>
            test <@ env'.ContainsKey("test") @>
            unbox env'.["test"] =? "value"
            test <@ env'.ContainsKey(Constants.requestMethod) @>
            unbox env'.[Constants.requestMethod] =? "GET"
        testCase "should return new instance with updated, existing key" <| fun _ ->
            let env = env()
            let responseBody = new MemoryStream("Hello, world"B)
            let env' = env.With(Constants.responseBody, responseBody :> System.IO.Stream)
            test <@ not (obj.ReferenceEquals(env, env')) @>
            test <@ not (obj.ReferenceEquals(env.ResponseBody, env'.ResponseBody)) @>
            test <@ env'.ContainsKey(Constants.responseBody) @>
            env'.ResponseBody.Length =? responseBody.Length
            (env'.ResponseBody :?> MemoryStream).ToArray() =? responseBody.ToArray()
            test <@ obj.ReferenceEquals(env'.ResponseBody, responseBody) @>
    ]

let adapterTests =
    testList "When creating an OwinAppFunc from a System.Web.Http function" [
        testCase "should create a new OwinAppFunc" <| fun _ ->
            let env = env()
            let handler request = async {
                let bytes = "Hello, world"B
                let content = new ByteArrayContent(bytes)
                content.Headers.ContentLength <- Nullable bytes.LongLength
                content.Headers.ContentType <- Headers.MediaTypeHeaderValue("text/plain")
                let response = new HttpResponseMessage(HttpStatusCode.OK, Content = content, RequestMessage = request)
                return response
            }

            let app = Dyfrig.Net.Http.fromAsyncSystemNetHttp handler

            async {
                do! app.Invoke(env).ContinueWith(Func<_,_>(fun _ -> ())) |> Async.AwaitTask
                env.ResponseStatusCode =? 200
                env.ResponseReasonPhrase =? "OK"
                env.ResponseHeaders.Count =? 2
                env.ResponseHeaders.["Content-Type"] =? [|"text/plain"|]
                env.ResponseHeaders.["Content-Length"] =? [|"12"|]
                // Test the response body
                env.ResponseBody <>? null
                env.ResponseBody.Position <- 0L
                let body = Array.zeroCreate 12
                let bytesRead = env.ResponseBody.Read(body, 0, int env.ResponseBody.Length)
                bytesRead =? 12
                body =? "Hello, world"B
            } |> Async.RunSynchronously
        testCase "should reproduce issue with incorrect status code" <| fun _ ->
            let env = env()
            let handler request = async {
                let bytes = "Hello, world"B
                let content = new ByteArrayContent(bytes)
                content.Headers.ContentLength <- Nullable bytes.LongLength
                content.Headers.ContentType <- Headers.MediaTypeHeaderValue("text/plain")
                let response = new HttpResponseMessage(HttpStatusCode.Created, Content = content, RequestMessage = request)
                return response
            }

            let app = Dyfrig.Net.Http.fromAsyncSystemNetHttp handler

            async {
                do! app.Invoke(env).ContinueWith(Func<_,_>(fun _ -> ())) |> Async.AwaitTask
                env.ResponseStatusCode =? 201
                env.ResponseReasonPhrase =? "Created"
                env.ResponseHeaders.Count =? 2
                env.ResponseHeaders.["Content-Type"] =? [|"text/plain"|]
                env.ResponseHeaders.["Content-Length"] =? [|"12"|]
                // Test the response body
                env.ResponseBody <>? null
                env.ResponseBody.Position <- 0L
                let body = Array.zeroCreate 12
                let bytesRead = env.ResponseBody.Read(body, 0, int env.ResponseBody.Length)
                bytesRead =? 12
                body =? "Hello, world"B
            } |> Async.RunSynchronously
    ]

[<EntryPoint>]
let main argv =
    [
        initializingTests
        environmentModuleTests
        adapterTests
    ]
    |> testList "Environment tests"
    |> runParallel
