namespace Dyfrig

open System
open System.Diagnostics.Contracts
open System.IO

/// `Stream` decorator to prevent `System.Net.Http` types from closing the provided `Stream` from the `Environment`.
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
    
/// Helper functions for working with an OWIN environment dictionary
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SystemNetHttpAdapter =

    open System.Net.Http
    open System.Threading.Tasks
    open Dyfrig
    open Environment

    /// Literal defintion for the `dyfrig.Environment` key.
    [<Literal>]
    let dyfrigEnvironment = "dyfrig.Environment"

    /// Converts the `OwinEnv` into an `HttpRequestMessage`.
    [<CompiledName("ToHttpRequestMesage")>]
    let toHttpRequestMessage (environment: OwinEnv) =
        let env = environment |> toEnvironment
        match env.GetRequestUri() with
        | Some requestUri ->
            let content = new StreamContent(new ProtectedStream(env.RequestBody))
            let request = new HttpRequestMessage(HttpMethod(env.RequestMethod), requestUri, Content = content)
            for header in env.RequestHeaders.Keys do
                request.Headers.TryAddWithoutValidation(header, env.RequestHeaders.[header]) |> ignore
            request.Version <- Version(env.RequestProtocol.Substring(5))
            request.Properties.Add(dyfrigEnvironment, box env)
            // TODO: Add additional, common properties here.
            Some request
        | None -> None
    
    /// Invokes an `HttpResponseMessage` in an OWIN handler.
    [<CompiledName("InvokeHttpResponseMessage")>]
    let invokeHttpResponseMessage (response: HttpResponseMessage) =
        Contract.Requires(response.RequestMessage <> null)
        Contract.Requires(response.RequestMessage.Properties.ContainsKey(dyfrigEnvironment))
        Contract.Requires(response.RequestMessage.Properties.[dyfrigEnvironment] <> null)
        Contract.Requires((response.RequestMessage.Properties.[dyfrigEnvironment] |> unbox<Environment>).ResponseBody <> null)

        let env = response.RequestMessage.Properties.[dyfrigEnvironment] :?> Environment
        env.ResponseStatusCode <- int response.StatusCode
        env.ResponseReasonPhrase <- response.ReasonPhrase
        // Copy response message headers
        for header in response.Headers do
            env.ResponseHeaders.Add(header.Key, header.Value |> Seq.toArray)
        async {
            if response.Content = null then
                // Set the Content-Length header
                env.ResponseHeaders.Add("Content-Length", [|"0"|])
            else
                // Copy response content headers
                for header in response.Content.Headers do
                    env.ResponseHeaders.Add(header.Key, header.Value |> Seq.toArray)
                // Finally, write the response body content
                // TODO: Handle the faulted state here.
                do! response.Content.CopyToAsync(env.ResponseBody).ContinueWith(Func<_,_>(fun _ -> ())) |> Async.AwaitTask
        }

    /// Adapts a function of type `HttpRequestMessage -> Async<HttpResponseMessage>` to an OWIN handler.
    [<CompiledName("FromAsyncSystemNetHttp")>]
    let fromAsyncSystemNetHttp (f: HttpRequestMessage -> Async<HttpResponseMessage>) =
        OwinAppFunc(fun env ->
            let request = env |> toHttpRequestMessage |> Option.get
            async {
                let! response = f request
                do! invokeHttpResponseMessage response
            }
            |> Async.StartAsTask
            :> Task)

    /// Adapts a function of type `HttpRequestMessage -> Task<HttpResponseMessage>` to an OWIN handler.
    [<CompiledName("FromSystemNetHttp")>]
    let fromSystemNetHttp (f: HttpRequestMessage -> Task<HttpResponseMessage>) =
        OwinAppFunc(fun env ->
            let request = env |> toHttpRequestMessage |> Option.get
            async {
                let! response = f request |> Async.AwaitTask
                do! invokeHttpResponseMessage response
            }
            |> Async.StartAsTask
            :> Task)
