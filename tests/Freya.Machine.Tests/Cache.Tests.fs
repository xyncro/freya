module Freya.Cache.Tests

open System
open System.Text
open Arachne.Http
open Arachne.Http.Cors
open Arachne.Uri.Template
open Freya.Core
open Freya.Core.Operators
open Freya.Inspector
open Freya.Machine
open Freya.Machine.Extensions.Http
open Freya.Machine.Extensions.Http.Cors
open Freya.Machine.Inspector
open Freya.Machine.Router
open Freya.Router
open Freya.Router.Inspector

open System.Net.Http
open Microsoft.Owin.Hosting
open Microsoft.Owin.Testing
open NUnit.Framework
open Swensen.Unquote
open Freya.Core.Integration

module Sut =
    let represent (x:string) =
        { Description =
            { Charset = Some Charset.Utf8
              Encodings = None
              MediaType = Some MediaType.Json
              Languages = None}
          Data =  x |> Encoding.UTF8.GetBytes }

    let cachingMachine = 
        freyaMachine {
            using http
            methodsSupported [OPTIONS;GET;PUT;]
            handleOk (represent "hello")
            respondWithEntity true
            etag (Strong("test"))
        } 
    let caching =  
        freyaRouter {
            resource (UriTemplate.parse "/") cachingMachine
         }

    type Katana () =
        member __.Configuration () =
            OwinAppFunc.ofFreya caching


module Tests =
    // Create test server and client[<Test>]
    let baseAddress = Uri "http://localhost/"
    let server = TestServer.Create<Sut.Katana>()
    server.BaseAddress <- baseAddress
    let client = server.HttpClient
    client.BaseAddress <- baseAddress


    [<Test>]
    let ``Returns 200 OK with no ETag`` () = 
        async {
            use request = new HttpRequestMessage(HttpMethod.Get, Uri "http://localhost/") 
            use! response = Async.AwaitTask <| client.SendAsync request
            let! result = Async.AwaitTask <| response.Content.ReadAsStringAsync()
            response.StatusCode =! Net.HttpStatusCode.OK
            result =! "hello" }      
        |> Async.RunSynchronously

    [<Test>]
    let ``Returns 304 NOT MODIFIED with matching ETag`` () = 
        async {
            use request = new HttpRequestMessage(HttpMethod.Get, Uri "http://localhost/") 
            request.Headers.Add("If-None-Match", ["\"test\""])
            use! response = Async.AwaitTask <| client.SendAsync request
            let! result = Async.AwaitTask <| response.Content.ReadAsStringAsync()
            response.StatusCode =! Net.HttpStatusCode.NotModified }      
        |> Async.RunSynchronously

    [<Test>]
    let ``Returns 200 OK with non-matching ETag`` () = 
        async {
            use request = new HttpRequestMessage(HttpMethod.Get, Uri "http://localhost/") 
            request.Headers.Add("If-None-Match", ["\"test-no-match\""])
            use! response = Async.AwaitTask <| client.SendAsync request
            let! result = Async.AwaitTask <| response.Content.ReadAsStringAsync()
            response.StatusCode =! Net.HttpStatusCode.NotModified 
            result =! "hello" }      
        |> Async.RunSynchronously

    [<Test>]
    let ``Allows PUT without ETag`` () =
        async {
            use request = new HttpRequestMessage(HttpMethod.Put, Uri "http://localhost/") 
            use! response = Async.AwaitTask <| client.SendAsync request
            let! result = Async.AwaitTask <| response.Content.ReadAsStringAsync()
            response.StatusCode =! Net.HttpStatusCode.Created}
        |> Async.RunSynchronously

    [<Test>]
    let ``Allows PUT with matching ETag`` () =
        async {
            use request = new HttpRequestMessage(HttpMethod.Put, Uri "http://localhost/") 
            request.Headers.Add("If-Match", ["\"test\""])
            use! response = Async.AwaitTask <| client.SendAsync request
            let! result = Async.AwaitTask <| response.Content.ReadAsStringAsync()
            response.StatusCode =! Net.HttpStatusCode.Created}
        |> Async.RunSynchronously

    [<Test>]
    let ``Disallows PUT with non-matching ETag`` () =
        async {
            use request = new HttpRequestMessage(HttpMethod.Put, Uri "http://localhost/") 
            request.Headers.Add("If-Match", ["\"test-no-match\""])
            use! response = Async.AwaitTask <| client.SendAsync request
            let! result = Async.AwaitTask <| response.Content.ReadAsStringAsync()
            response.StatusCode =! Net.HttpStatusCode.PreconditionFailed}
        |> Async.RunSynchronously




