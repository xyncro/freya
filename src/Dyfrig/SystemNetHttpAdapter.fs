//----------------------------------------------------------------------------
//
// Copyright (c) 2013-2014 Ryan Riley (@panesofglass)
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
namespace Dyfrig

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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SystemNetHttpAdapter =

    open System.Collections.Generic
    open System.Net.Http
    open System.Threading.Tasks
    open Dyfrig
    open Environment

    [<CompiledName("DyfrigEnvironment")>]
    let [<Literal>] dyfrigEnvironment = "dyfrig.Environment"

    let mapEnvToRequest (requestUri: string) (env: Environment) =
        let content = new StreamContent(new ProtectedStream(env.RequestBody))
        let request = new HttpRequestMessage(HttpMethod(env.RequestMethod), requestUri, Content = content)
        for header in env.RequestHeaders.Keys do
            request.Headers.TryAddWithoutValidation(header, env.RequestHeaders.[header]) |> ignore
        request.Version <- Version(env.RequestProtocol.Substring(5))
        request.Properties.Add(dyfrigEnvironment, box env)
        // TODO: Add additional, common properties here.
        request

    [<CompiledName("ToHttpRequestMesage")>]
    let toHttpRequestMessage (environment: OwinEnv) =
        assert(environment <> null)

        let env = environment |> toEnvironment
        match env.GetRequestUri() with
        | Some requestUri -> Some (mapEnvToRequest requestUri env)
        | None -> None
    
    [<CompiledName("InvokeHttpResponseMessage")>]
    let invokeHttpResponseMessage (environment: OwinEnv) (response: HttpResponseMessage) =
        assert(environment <> null)
        assert(response <> null)
        assert(response.RequestMessage <> null)

        let env = environment |> toEnvironment
        env.ResponseStatusCode <- int response.StatusCode
        env.ResponseReasonPhrase <- response.ReasonPhrase
        // Copy response message headers
        for header in response.Headers do
            env.ResponseHeaders.[header.Key] <- header.Value |> Seq.toArray
        async {
            if response.Content = null then
                // Set the Content-Length header
                env.ResponseHeaders.["Content-Length"] <- [|"0"|]
            else
                // Copy response content headers
                for header in response.Content.Headers do
                    env.ResponseHeaders.[header.Key] <- header.Value |> Seq.toArray
                // Finally, write the response body content
                // TODO: Handle the faulted state here.
                do! response.Content.CopyToAsync(env.ResponseBody).ContinueWith(Func<_,_>(fun _ -> ())) |> Async.AwaitTask
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

    [<CompiledName("ToHttpRequestRailway")>]
    let toHttpRequestRailway (environment: OwinEnv) =
        if environment = null
        then Choice2Of2(nullArg "environment" "environment cannot be null" :> exn) |> async.Return
        else
        match toHttpRequestMessage environment with
        | Some request -> Choice1Of2 request
        | None -> Choice2Of2 (invalidArg "environment" "environment did not contain a complete request URI" :> exn)
        |> async.Return

    [<CompiledName("MapResponseToEnvironment")>]
    let mapResponseToEnvironment (environment: OwinEnv) (response: HttpResponseMessage) = async {
        assert(environment <> null)
        let env = environment |> toEnvironment

        // Collect response headers.
        let headers = Dictionary<string, string[]>(StringComparer.Ordinal)
        for header in response.Headers do
            headers.[header.Key] <- header.Value |> Seq.toArray

        // Process response body.
        let headers', outStream =
            if response.Content = null then
                headers.["Content-Length"] <- [|"0"|]
                headers, async.Return Stream.Null
            else
                for header in response.Content.Headers do
                    headers.[header.Key] <- header.Value |> Seq.toArray
                let out = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
                headers, out

        // Create return Environment.
        let! body = outStream
        let env' =
            env.With(Constants.responseStatusCode, int response.StatusCode)
               .With(Constants.responseReasonPhrase, response.ReasonPhrase)
               .With(Constants.responseHeaders, headers)
               .With(Constants.responseBody, body)
        return env'
    }
