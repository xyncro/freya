//----------------------------------------------------------------------------
//
// Copyright (c) 2014
//
//    Ryan Riley (@panesofglass) and Andrew Cherry (@kolektiv)
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
//----------------------------------------------------------------------------

module Freya.Machine.Tests

open System
open System.Net.Http
open Microsoft.Owin.Hosting
open Microsoft.Owin.Testing
open NUnit.Framework
open Swensen.Unquote
open Freya.Core
open Freya.Core.Integration
open Freya.TodoBackend

(* Katana

   Katana (Owin Self Hosting) expects us to expose a type with a specific
   method. Freya lets us do see easily, the OwinAppFunc module providing
   functions to turn any Freya<'a> function in to a suitable value for
   OWIN compatible hosts such as Katana. *)

type TodoBackend () =
    member __.Configuration () =
        OwinAppFunc.ofFreya Freya.TodoBackend.Api.api

(* Main

   A very simple program, simply a console app, with a blocking read from
   the console to keep our server from shutting down immediately. Though
   we are self hosting here as a console application, the same application
   should be easily transferrable to any OWIN compatible server, including
   IIS. *)


// Create test server and client
let baseAddress = Uri "http://localhost/"
let server = TestServer.Create<TodoBackend>()
server.BaseAddress <- baseAddress
let client = server.HttpClient
client.BaseAddress <- baseAddress

// Add common CORS headers
let addOriginHeader (domain:string) (request:System.Net.Http.HttpRequestMessage) =
    request.Headers.Add("Origin", domain)
    request

let addDefaultOrigin request =
    addOriginHeader "http://example.org" request

// This seems to cause interesting timing race conditions with more complex tests,
// which are inherently async...

//[<TestFixtureTearDown>]
//let dispose() =
//    client.Dispose()
//    server.Dispose()

[<Test>]
let ``todobackend returns empty array at first`` () = 
    async {
        use request = new HttpRequestMessage(HttpMethod.Get, Uri "http://localhost/") |> addDefaultOrigin
        use! response = Async.AwaitTask <| client.SendAsync request
        let! result = Async.AwaitTask <| response.Content.ReadAsStringAsync()
        response.StatusCode =! Net.HttpStatusCode.OK
        let headers =
            response.Headers
            |> Seq.map (fun (KeyValue(k,v)) -> k, List.ofSeq v)
            |> Map.ofSeq
        response.Headers.Date.HasValue   =! true
        response.Headers.Date.Value.Date =! DateTimeOffset.UtcNow.Date
        headers.["Access-Control-Expose-Headers"] =! [""]
        headers.["Access-Control-Allow-Origin"]   =! ["http://example.org"]
        result =! "[]" }
    |> Async.RunSynchronously

[<Test>]
let ``todobackend returns expected headers for OPTIONS request`` () =
    async {
        use request = new HttpRequestMessage(HttpMethod.Options, Uri "http://localhost/") |> addDefaultOrigin
        use! response = Async.AwaitTask <| client.SendAsync request
        response.StatusCode =! Net.HttpStatusCode.OK
        let headers =
            response.Headers
            |> Seq.map (fun (KeyValue(k,v)) -> k, List.ofSeq v)
            |> Map.ofSeq
        response.Headers.Date.HasValue   =! true
        response.Headers.Date.Value.Date =! DateTimeOffset.UtcNow.Date
        headers.["Access-Control-Expose-Headers"] =! [""]
        headers.["Access-Control-Allow-Origin"]   =! ["http://example.org"] }
    |> Async.RunSynchronously

[<Test>]
let ``todobackend blocks CORS request from unacceptable origin`` () =
    async {
        use request = new HttpRequestMessage(HttpMethod.Options, Uri "http://localhost/") |> addOriginHeader "http://evil.org/"
        use! response = Async.AwaitTask <| client.SendAsync request
        // TODO: is this correct?
        response.StatusCode =! Net.HttpStatusCode.OK
        let headers =
            response.Headers
            |> Seq.map (fun (KeyValue(k,v)) -> k, List.ofSeq v)
            |> Map.ofSeq
        response.Headers.Date.HasValue   =! true
        response.Headers.Date.Value.Date =! DateTimeOffset.UtcNow.Date
        headers.ContainsKey("Access-Control-Expose-Headers") =! false
        headers.ContainsKey("Access-Control-Allow-Origin")   =! false }
    |> Async.RunSynchronously

(* If-Modified-Since tests *)

let private requestWithIfModifiedSince dayDiff = 
    let request = new HttpRequestMessage(HttpMethod.Get, Uri "http://localhost/") |> addDefaultOrigin
    let testModificationDate =
        (DateTimeOffset Api.lastModificationDate).AddDays dayDiff
    request.Headers.IfModifiedSince <- Nullable testModificationDate
    request

[<Test>]
let ``todobackend returns 304 for If-Modified-Since`` () = 
    async {
        use request = requestWithIfModifiedSince 1.0
        use! response = Async.AwaitTask <| client.SendAsync request
        response.StatusCode =! Net.HttpStatusCode.NotModified

        let! result = Async.AwaitTask <| response.Content.ReadAsStringAsync()
        result =! "" }
    |> Async.RunSynchronously

[<Test>]
let ``todobackend not returns 200 for If-Modified-Since`` () = 
    async {
        use request = requestWithIfModifiedSince -1.0
        use! response = Async.AwaitTask <| client.SendAsync request
        response.StatusCode =! Net.HttpStatusCode.OK

        let! result = Async.AwaitTask <| response.Content.ReadAsStringAsync()
        result =! "[]" }
    |> Async.RunSynchronously
