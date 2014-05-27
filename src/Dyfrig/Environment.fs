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
open System.Collections.Generic
open System.IO
open System.Linq
open System.Net
open System.Threading
open System.Threading.Tasks
open Microsoft.FSharp.Core

/// OWIN headers dictionary
type OwinHeaders = IDictionary<string, string[]>

/// An Environment dictionary to store OWIN request and response values.
type Environment =
    inherit Dictionary<string, obj>

    val private requestHeaders   : OwinHeaders
    val private requestBody      : Stream
    val private responseHeaders  : OwinHeaders
    val private responseBody     : Stream
    val private callCancelled    : CancellationToken
    val private owinVersion      : string

    val mutable private disposed : bool

    /// Initializes a new Environment from an existing, valid, OWIN environment dictionary.
    new (dictionary: OwinEnv) =
        {
            inherit Dictionary<string, obj>(dictionary, StringComparer.Ordinal)
            disposed = false
            requestHeaders = unbox dictionary.[Constants.requestHeaders]
            requestBody = unbox dictionary.[Constants.requestBody]
            responseHeaders = unbox dictionary.[Constants.responseHeaders]
            responseBody = unbox dictionary.[Constants.responseBody]
            callCancelled = unbox dictionary.[Constants.callCancelled]
            owinVersion = unbox dictionary.[Constants.owinVersion]
        }

    /// Initializes a new Environment from parameters, adding defaults for optional response parameters.
    new (requestMethod: string, requestScheme: string, requestPathBase: string, requestPath: string, requestQueryString: string, requestProtocol: string, requestHeaders, ?requestBody, ?responseHeaders, ?responseBody, ?callCancelled) as x =
        // TODO: Consider parsing the URI rather than requiring the pieces to be passed in explicitly.
        {
            inherit Dictionary<string, obj>(StringComparer.Ordinal)
            disposed = false
            requestHeaders = requestHeaders
            requestBody = defaultArg requestBody Stream.Null
            responseHeaders = defaultArg responseHeaders (new Dictionary<_,_>(HashIdentity.Structural) :> IDictionary<_,_>)
            responseBody = defaultArg responseBody (new MemoryStream() :> Stream)
            callCancelled = defaultArg callCancelled (let cts = new CancellationTokenSource() in cts.Token)
            owinVersion = "1.0"
        }
        then do
            x.Add(Constants.requestMethod, requestMethod)
            x.Add(Constants.requestScheme, requestScheme)
            x.Add(Constants.requestPathBase, requestPathBase)
            x.Add(Constants.requestPath, requestPath)
            x.Add(Constants.requestQueryString, requestQueryString)
            x.Add(Constants.requestProtocol, requestProtocol)
            x.Add(Constants.requestHeaders, x.requestHeaders)
            x.Add(Constants.requestBody, x.requestBody)
            x.Add(Constants.responseHeaders, x.responseHeaders)
            x.Add(Constants.responseBody, x.responseBody)
            x.Add(Constants.callCancelled, x.callCancelled)
            x.Add(Constants.owinVersion, x.owinVersion)

    /// Gets a value with the specified key from the environment dictionary as the specified type 'a.
    static member inline Get<'a> (environment: OwinEnv, key: string) =
        if environment.ContainsKey(key) then
            Some(environment.[key] :?> 'a)
        else None

    /// Gets the HTTP method used in the current request.
    member x.RequestMethod
        with get() : string = unbox x.[Constants.requestMethod]
        and set(v : string) = x.[Constants.requestMethod] <- v

    /// Gets the scheme (e.g. "http" or "https") for the current request.
    member x.RequestScheme
        with get() : string = unbox x.[Constants.requestScheme]
        and set(v : string) = x.[Constants.requestScheme] <- v

    /// Gets the path corresponding to the "root" of the application.
    member x.RequestPathBase
        with get() : string = unbox x.[Constants.requestPathBase]
        and set(v : string) = x.[Constants.requestPathBase] <- v

    /// Gets the path relative to the "root" of the application.
    member x.RequestPath
        with get() : string = unbox x.[Constants.requestPath]
        and set(v : string) = x.[Constants.requestPath] <- v

    /// Gets the query string from the request URI.
    member x.RequestQueryString
        with get() : string = unbox x.[Constants.requestQueryString]
        and set(v : string) = x.[Constants.requestQueryString] <- v

    /// Gets the HTTP protocol version for the request.
    member x.RequestProtocol
        with get() : string = unbox x.[Constants.requestProtocol]
        and set(v : string) = x.[Constants.requestProtocol] <- v
    
    /// Gets the request headers dictionary for the current request.
    member x.RequestHeaders = x.requestHeaders

    /// Reconstructs the base request URI from the component parts.
    member env.GetBaseUri() =
        if env.RequestHeaders.ContainsKey("Host") then
            env.RequestScheme + "://" +
            (env.RequestHeaders.["Host"].[0]) +
            if String.IsNullOrEmpty env.RequestPathBase then "/" else env.RequestPathBase
            |> Some
        else None

    /// Reconstructs the request URI from the component parts.
    member env.GetRequestUri() =
        if env.RequestHeaders.ContainsKey("Host") then
            env.RequestScheme + "://" +
            (env.RequestHeaders.["Host"].[0]) +
            env.RequestPathBase +
            env.RequestPath +
            if String.IsNullOrEmpty env.RequestQueryString then "" else "?" + env.RequestQueryString
            |> Some
        else None
 
    /// Gets the request body for the current request.
    member x.RequestBody = x.requestBody

    /// Gets the response status code for the current request.
    member x.ResponseStatusCode
        with get() : int =
            if x.ContainsKey(Constants.responseStatusCode) then
                unbox x.[Constants.responseStatusCode]
            else 200 // Default for HTTP 200 OK
        and set(v : int) = x.[Constants.responseStatusCode] <- v

    /// Gets the response reason phrase for the current request.
    member x.ResponseReasonPhrase
        with get() : string =
            if x.ContainsKey(Constants.responseReasonPhrase) then
                unbox x.[Constants.responseReasonPhrase]
            elif x.ContainsKey(Constants.responseStatusCode) then
                unbox x.[Constants.responseStatusCode] |> enum<HttpStatusCode> |> string
            else "OK" // Default for HTTP 200 OK
        and set(v : string) = x.[Constants.responseReasonPhrase] <- v

    /// Gets the response status code for the current request.
    member x.ResponseProtocol
        with get() : string option =
            if x.ContainsKey(Constants.responseProtocol) then
                Some(unbox x.[Constants.responseProtocol])
            else None
        and set(v : string option) =
            match v with
            | Some v -> x.[Constants.responseProtocol] <- v
            | None -> x.Remove(Constants.responseProtocol) |> ignore

    /// Gets the response headers dictionary for the current response.
    member x.ResponseHeaders = x.responseHeaders

    /// Gets the response body stream.
    member x.ResponseBody = x.responseBody

    /// Gets or sets the `CancellationToken` indicating whether the request has been cancelled.
    member x.CallCancelled
        with get() : int = unbox x.[Constants.callCancelled]
        and set(v : int) = x.[Constants.callCancelled] <- v

    /// Gets the current OWIN version.
    member x.OwinVersion = x.owinVersion

    /// Overridable disposal implementation for this instance.
    abstract Dispose : bool -> unit
    default x.Dispose(disposing) =
        if disposing then
            x.Values.OfType<IDisposable>()
            |> Seq.filter (fun x -> x <> Unchecked.defaultof<_>)
            |> Seq.iter (fun x -> try x.Dispose() with | _ -> ()) // TODO: Log any failed disposals.

    /// Disposes this instance.
    member x.Dispose() =
        if not x.disposed then
            GC.SuppressFinalize(x)
            x.Dispose(true)
            x.disposed <- true

    interface IDisposable with
        /// Disposes this instance.
        member x.Dispose() = x.Dispose()

/// Helper functions for working with an OWIN environment dictionary
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Environment =

    /// Returns an Environment from an `OwinEnv`.
    /// If the dictionary is already an Environment, then the instance is cast and returned
    /// rather than mapped into a new instance.
    [<CompiledName("ToEnvironment")>]
    let toEnvironment (environment: OwinEnv) =
        match environment with
        | :? Environment as e -> e
        | _ as d -> new Environment(d)
    
/// Helper functions for working with an OWIN environment dictionary
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SystemNetHttpAdapter =

    open System.Diagnostics.Contracts
    open System.Net.Http
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
            let content = new StreamContent(env.RequestBody)
            let request = new HttpRequestMessage(HttpMethod(env.RequestMethod), requestUri, Content = content)
            for header in env.RequestHeaders.Keys do
                request.Headers.TryAddWithoutValidation(header, env.RequestHeaders.[header]) |> ignore
            request.Version <- Version(env.RequestProtocol.Substring(5))
            request.Properties.Add(dyfrigEnvironment, env)
            request.Properties.Add(Constants.callCancelled, env.CallCancelled)
            // TODO: Add additional, common properties here.
            Some request
        | None -> None
    
    /// Invokes an `HttpResponseMessage` in an OWIN handler.
    [<CompiledName("InvokeHttpResponseMessage")>]
    let invokeHttpResponseMessage (response: HttpResponseMessage) =
        Contract.Requires(response.RequestMessage <> null)
        Contract.Requires(response.RequestMessage.Properties.ContainsKey(dyfrigEnvironment))
        Contract.Requires(response.RequestMessage.Properties.[dyfrigEnvironment] <> null)
        Contract.Requires((response.RequestMessage.Properties.[dyfrigEnvironment] :?> Environment).ResponseBody <> null)

        let env = response.RequestMessage.Properties.[dyfrigEnvironment] :?> Environment
        env.ResponseStatusCode <- int response.StatusCode
        env.ResponseReasonPhrase <- response.StatusCode.ToString()
        for header in response.Headers do
            env.ResponseHeaders.Add(header.Key, header.Value |> Seq.toArray)
        response.Content.CopyToAsync(env.ResponseBody)

    /// Adapts a function of type `HttpRequestMessage -> Async<HttpResponseMessage>` to an OWIN handler.
    [<CompiledName("FromAsyncSystemNetHttp")>]
    let fromAsyncSystemNetHttp (f: HttpRequestMessage -> Async<HttpResponseMessage>) =
        OwinAppFunc(fun env ->
            let request = env |> toHttpRequestMessage |> Option.get
            async {
                let! response = f request
                do! (invokeHttpResponseMessage response).ContinueWith(Func<_,_>(fun _ -> ())) |> Async.AwaitTask
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
                do! (invokeHttpResponseMessage response).ContinueWith(Func<_,_>(fun _ -> ())) |> Async.AwaitTask
            }
            |> Async.StartAsTask
            :> Task)
