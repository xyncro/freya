//----------------------------------------------------------------------------
//
// Copyright (c) 2013-2014
//
//    Ryan Riley (@panesofglass)
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
//----------------------------------------------------------------------------
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Freya.Net.Http

open System
open System.IO

[<Sealed>]
type ProtectedStream(innerStream: Stream) =
    inherit Stream()
    do if innerStream = null then raise (ArgumentNullException("innerStream"))

    let mutable disposed = false

    let raiseIfDisposed () =
        if disposed then raise (ObjectDisposedException(null))
    
    override x.CanRead = if disposed then false else innerStream.CanRead
    override x.CanSeek = if disposed then false else innerStream.CanSeek
    override x.CanTimeout = innerStream.CanTimeout
    override x.CanWrite = if disposed then false else innerStream.CanWrite
    override x.Length = raiseIfDisposed(); innerStream.Length
    override x.Position
        with get() = raiseIfDisposed(); innerStream.Position
        and set(v) = raiseIfDisposed(); innerStream.Position <- v
    override x.ReadTimeout
        with get() = raiseIfDisposed(); innerStream.ReadTimeout
        and set(v) = raiseIfDisposed(); innerStream.ReadTimeout <- v
    override x.WriteTimeout
        with get() = raiseIfDisposed(); innerStream.WriteTimeout
        and set(v) = raiseIfDisposed(); innerStream.WriteTimeout <- v
    override x.BeginRead(buffer, offset, count, callback, state) =
        raiseIfDisposed()
        innerStream.BeginRead(buffer, offset, count, callback, state)
    override x.BeginWrite(buffer, offset, count, callback, state) =
        raiseIfDisposed()
        innerStream.BeginWrite(buffer, offset, count, callback, state)
    // Ensure we don't close the `innerStream`.
    override x.Close() = base.Close() 
    // Ensure we don't close the `innerStream`.
    override x.Dispose(disposing) = if not disposed then base.Dispose(disposing); disposed <- true
    override x.EndRead(asyncResult) = raiseIfDisposed(); innerStream.EndRead(asyncResult)
    override x.EndWrite(asyncResult) = raiseIfDisposed(); innerStream.EndWrite(asyncResult)
    override x.Flush() = raiseIfDisposed(); innerStream.Flush();
    override x.Read(buffer, offset, count) = raiseIfDisposed(); innerStream.Read(buffer, offset, count);
    override x.ReadByte() = raiseIfDisposed(); innerStream.ReadByte();
    override x.Seek(offset, origin) = raiseIfDisposed(); innerStream.Seek(offset, origin)
    override x.SetLength(value) = raiseIfDisposed(); innerStream.SetLength(value)
    override x.Write(buffer, offset, count) = raiseIfDisposed(); innerStream.Write(buffer, offset, count)
    override x.WriteByte(value) = raiseIfDisposed(); innerStream.WriteByte(value)

open System.Collections.Generic
open System.Net.Http
open System.Threading.Tasks
open Freya.Core

[<CompiledName("FreyaEnvironment")>]
let [<Literal>] dyfrigEnvironment = "dyfrig.Environment"

let mapEnvToRequest (requestUri: string) (env: FreyaEnvironment) =
    let content = new StreamContent(new ProtectedStream(unbox<Stream> env.[Constants.requestBody]))
    let request = new HttpRequestMessage(HttpMethod(unbox<string> env.[Constants.requestMethod]), requestUri, Content = content)
    let requestHeaders = unbox<IDictionary<string, string[]>> env.[Constants.requestHeaders]
    for header in requestHeaders.Keys do
        request.Headers.TryAddWithoutValidation(header, requestHeaders.[header]) |> ignore
    let requestProtocol = unbox<string> env.[Constants.requestProtocol]
    request.Version <- Version(requestProtocol.Substring(5))
    request.Properties.Add(dyfrigEnvironment, box env)
    // TODO: Add additional, common properties here.
    request

// TODO: Replace with Lens-based approach

let requestHeaders (env: FreyaEnvironment) =
    unbox<IDictionary<string, string[]>> env.[Constants.requestHeaders]

let baseUri env =
    let requestHeaders = requestHeaders env
    if requestHeaders.ContainsKey("Host") then
        (unbox<_> env.[Constants.requestScheme]) + "://" +
        requestHeaders.["Host"].[0] +
        if String.IsNullOrEmpty (unbox env.[Constants.requestPathBase]) then "/" else (unbox env.[Constants.requestPathBase])
        |> Some
    else None

let requestUri (env: FreyaEnvironment) =
    let requestHeaders = requestHeaders env
    if requestHeaders.ContainsKey("Host") then
        (unbox env.[Constants.requestScheme]) + "://" +
        requestHeaders.["Host"].[0] +
        (unbox env.[Constants.requestPathBase]) +
        (unbox env.[Constants.requestPath]) +
        if String.IsNullOrEmpty (unbox env.[Constants.requestQueryString]) then "/" else (unbox env.[Constants.requestQueryString])
        |> Some
    else None

[<CompiledName("ToHttpRequestMesage")>]
let toHttpRequestMessage (environment: FreyaEnvironment) =
    assert(environment <> null)

    match requestUri environment with
    | Some requestUri -> Some (mapEnvToRequest requestUri environment)
    | None -> None

[<CompiledName("InvokeHttpResponseMessage")>]
let invokeHttpResponseMessage (environment: FreyaEnvironment) (response: HttpResponseMessage) =
    assert(environment <> null)
    assert(response <> null)
    assert(response.RequestMessage <> null)

    environment.[Constants.responseStatusCode] <- int response.StatusCode
    environment.[Constants.responseReasonPhrase] <- response.ReasonPhrase
    // Copy response message headers
    let responseHeaders = unbox<IDictionary<string, string[]>> environment.[Constants.responseHeaders]
    for header in response.Headers do
        responseHeaders.[header.Key] <- header.Value |> Seq.toArray
    async {
        if response.Content = null then
            // Set the Content-Length header
            responseHeaders.["Content-Length"] <- [|"0"|]
        else
            // Copy response content headers
            for header in response.Content.Headers do
                responseHeaders.[header.Key] <- header.Value |> Seq.toArray
            // Finally, write the response body content
            // TODO: Handle the faulted state here.
            do! response.Content.CopyToAsync(unbox environment.[Constants.responseBody]).ContinueWith(Func<_,_>(fun _ -> ()))
                |> Async.AwaitTask
    }

[<CompiledName("FromAsyncSystemNetHttp")>]
let fromAsyncSystemNetHttp (handler: HttpRequestMessage -> Async<HttpResponseMessage>) =
    OwinAppFunc(fun env ->
        let request = env |> toHttpRequestMessage |> Option.get
        async {
            let! response = handler request
            do! invokeHttpResponseMessage env response
        }
        |> Async.StartAsTask
        :> Task)

[<CompiledName("FromSystemNetHttp")>]
let fromSystemNetHttp (handler: HttpRequestMessage -> Task<HttpResponseMessage>) =
    OwinAppFunc(fun env ->
        let request = env |> toHttpRequestMessage |> Option.get
        async {
            let! response = handler request |> Async.AwaitTask
            do! invokeHttpResponseMessage env response
        }
        |> Async.StartAsTask
        :> Task)
