//----------------------------------------------------------------------------
//
// Copyright (c) 2013 Ryan Riley (@panesofglass)
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

(**
 * OWIN 1.0.0
 * http://owin.org/spec/owin-1.0.0.html
 *)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Constants =
    (* 3.2.1 Request Data *)
    [<CompiledName("RequestScheme")>]
    let [<Literal>] requestScheme = "owin.RequestScheme"
    [<CompiledName("RequestMethod")>]
    let [<Literal>] requestMethod = "owin.RequestMethod"
    [<CompiledName("RequestPathBase")>]
    let [<Literal>] requestPathBase = "owin.RequestPathBase"
    [<CompiledName("RequestPath")>]
    let [<Literal>] requestPath = "owin.RequestPath"
    [<CompiledName("RequestQueryString")>]
    let [<Literal>] requestQueryString = "owin.RequestQueryString"
    [<CompiledName("RequestProtocol")>]
    let [<Literal>] requestProtocol = "owin.RequestProtocol"
    [<CompiledName("RequestHeaders")>]
    let [<Literal>] requestHeaders = "owin.RequestHeaders"
    [<CompiledName("RequestBody")>]
    let [<Literal>] requestBody = "owin.RequestBody"

    (* 3.2.2 Response Data *)
    [<CompiledName("ResponseStatusCode")>]
    let [<Literal>] responseStatusCode = "owin.ResponseStatusCode"
    [<CompiledName("ResponseReasonPhrase")>]
    let [<Literal>] responseReasonPhrase = "owin.ResponseReasonPhrase"
    [<CompiledName("ResponseProtocol")>]
    let [<Literal>] responseProtocol = "owin.ResponseProtocol"
    [<CompiledName("ResponseHeaders")>]
    let [<Literal>] responseHeaders = "owin.ResponseHeaders"
    [<CompiledName("ResponseBody")>]
    let [<Literal>] responseBody = "owin.ResponseBody"

    (* 3.2.3 Other Data *)
    [<CompiledName("CallCancelled")>]
    let [<Literal>] callCancelled = "owin.CallCancelled"
    [<CompiledName("OwinVersion")>]
    let [<Literal>] owinVersion = "owin.Version"

    (* http://owin.org/spec/CommonKeys.html *)
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module CommonKeys =
        [<CompiledName("ClientCertificate")>]
        let [<Literal>] clientCertificate = "ssl.ClientCertificate"
        [<CompiledName("RemoteIpAddress")>]
        let [<Literal>] remoteIpAddress = "server.RemoteIpAddress"
        [<CompiledName("RemotePort")>]
        let [<Literal>] remotePort = "server.RemotePort"
        [<CompiledName("LocalIpAddress")>]
        let [<Literal>] localIpAddress = "server.LocalIpAddress"
        [<CompiledName("LocalPort")>]
        let [<Literal>] localPort = "server.LocalPort"
        [<CompiledName("IsLocal")>]
        let [<Literal>] isLocal = "server.IsLocal"
        [<CompiledName("TraceOutput")>]
        let [<Literal>] traceOutput = "host.TraceOutput"
        [<CompiledName("Addresses")>]
        let [<Literal>] addresses = "host.Addresses"
        [<CompiledName("Capabilities")>]
        let [<Literal>] capabilities = "server.Capabilities"
        [<CompiledName("OnSendingHeaders")>]
        let [<Literal>] onSendingHeaders = "server.OnSendingHeaders"

    (* http://owin.org/extensions/owin-SendFile-Extension-v0.3.0.htm *)
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SendFiles =
        // 3.1. Startup
        [<CompiledName("Version")>]
        let [<Literal>] version = "sendfile.Version"
        [<CompiledName("Support")>]
        let [<Literal>] support = "sendfile.Support"
        [<CompiledName("Concurrency")>]
        let [<Literal>] concurrency = "sendfile.Concurrency"

        // 3.2. Per Request
        [<CompiledName("SendAsync")>]
        let [<Literal>] sendAsync = "sendfile.SendAsync"

    (* http://owin.org/extensions/owin-OpaqueStream-Extension-v0.3.0.htm *)
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Opaque =
        // 3.1. Startup
        [<CompiledName("Version")>]
        let [<Literal>] version = "opaque.Version"

        // 3.2. Per Request
        [<CompiledName("Upgrade")>]
        let [<Literal>] upgrade = "opaque.Upgrade"

        // 5. Consumption
        [<CompiledName("Stream")>]
        let [<Literal>] stream = "opaque.Stream"
        [<CompiledName("CallCanceled")>]
        let [<Literal>] callCancelled = "opaque.CallCancelled"

    // http://owin.org/extensions/owin-OpaqueStream-Extension-v0.3.0.htm
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module WebSocket =
        // 3.1. Startup
        [<CompiledName("Version")>]
        let [<Literal>] version = "websocket.Version"

        // 3.2. Per Request
        [<CompiledName("Accept")>]
        let [<Literal>] accept = "websocket.Accept"

        // 4. Accept
        [<CompiledName("SubProtocol")>]
        let [<Literal>] subProtocol = "websocket.SubProtocol"

        // 5. Consumption
        [<CompiledName("SendAsync")>]
        let [<Literal>] sendAsync = "websocket.SendAsync"
        [<CompiledName("ReceiveAsync")>]
        let [<Literal>] receiveAsync = "websocket.ReceiveAsync"
        [<CompiledName("CloseAsync")>]
        let [<Literal>] closeAsync = "websocket.CloseAsync"
        [<CompiledName("CallCancelled")>]
        let [<Literal>] callCancelled = "websocket.CallCancelled"
        [<CompiledName("ClientCloseStatus")>]
        let [<Literal>] clientCloseStatus = "websocket.ClientCloseStatus"
        [<CompiledName("ClientCloseDescription")>]
        let [<Literal>] clientCloseDescription = "websocket.ClientCloseDescription"

/// An Environment dictionary to store OWIN request and response values.
type Environment =
    inherit Dictionary<string, obj>

    val private requestMethod      : string
    val private requestScheme      : string
    val private requestPathBase    : string
    val private requestPath        : string
    val private requestQueryString : string
    val private requestProtocol    : string
    val private requestHeaders     : IDictionary<string, string[]>
    val private requestBody        : Stream
    val private responseHeaders    : IDictionary<string, string[]>
    val private responseBody       : Stream

    val mutable private responseStatusCode : HttpStatusCode
    val mutable private disposed : bool

    /// Initializes a new Environment from an existing, valid, OWIN environment dictionary.
    new (dictionary: IDictionary<_,_>) as x =
        {
            inherit Dictionary<string, obj>(dictionary, StringComparer.Ordinal)
            disposed = false
            requestMethod = unbox x.[Constants.requestMethod]
            requestScheme = unbox x.[Constants.requestScheme]
            requestPathBase = unbox x.[Constants.requestPathBase]
            requestPath = unbox x.[Constants.requestPath]
            requestQueryString = unbox x.[Constants.requestQueryString]
            requestProtocol = unbox x.[Constants.requestProtocol]
            requestHeaders = unbox x.[Constants.requestHeaders]
            requestBody = unbox x.[Constants.requestBody]
            responseStatusCode = if x.ContainsKey(Constants.responseStatusCode) then
                                     enum<_> <| unbox<int> x.[Constants.responseStatusCode]
                                 else HttpStatusCode.NotFound
            responseHeaders = unbox x.[Constants.responseHeaders]
            responseBody = unbox x.[Constants.responseBody]
        }
        then do
            x.[Constants.responseReasonPhrase] <- string x.responseStatusCode

    /// Initializes a new Environment from a request headers dictionary and body stream with default response headers and body.
    new (requestMethod, requestScheme, requestPathBase, requestPath, requestQueryString, requestProtocol, requestHeaders, ?requestBody, ?responseBody) as x =
        {
            inherit Dictionary<string, obj>(StringComparer.Ordinal)
            disposed = false
            requestMethod = requestMethod
            requestScheme = requestScheme
            requestPathBase = requestPathBase
            requestPath = requestPath
            requestQueryString = requestQueryString
            requestProtocol = requestProtocol
            requestHeaders = requestHeaders
            requestBody = defaultArg requestBody Stream.Null
            responseStatusCode = HttpStatusCode.NotFound
            responseHeaders = new Dictionary<_,_>(HashIdentity.Structural)
            responseBody = defaultArg responseBody (new MemoryStream() :> Stream)
        }
        then do
            x.Add(Constants.requestMethod, x.requestMethod)
            x.Add(Constants.requestScheme, x.requestScheme)
            x.Add(Constants.requestPathBase, x.requestPathBase)
            x.Add(Constants.requestPath, x.requestPath)
            x.Add(Constants.requestQueryString, x.requestQueryString)
            x.Add(Constants.requestProtocol, x.requestProtocol)
            x.Add(Constants.requestHeaders, x.requestHeaders)
            x.Add(Constants.requestBody, x.requestBody)
            x.Add(Constants.responseStatusCode, int x.responseStatusCode)
            x.Add(Constants.responseReasonPhrase, string x.responseStatusCode)
            x.Add(Constants.responseHeaders, x.responseHeaders)
            x.Add(Constants.responseBody, x.responseBody)

    /// Gets a value with the specified key from the environment dictionary as the specified type 'a.
    static member inline Get<'a> (environment: IDictionary<string, obj>, key: string) =
        if environment.ContainsKey(key) then
            Some(environment.[key] :?> 'a)
        else None

    /// Gets the request headers dictionary for the current request.
    member x.RequestHeaders = x.requestHeaders

    /// Gets the request body for the current request.
    member x.RequestBody = x.requestBody

    /// Gets the response status code for the current request.
    member x.ResponseStatusCode
        with get() = x.responseStatusCode
        and set(v) = x.responseStatusCode <- v
                     x.[Constants.responseStatusCode] <- int v
                     x.[Constants.responseReasonPhrase] <- string v

    /// Gets the response headers dictionary for the current response.
    member x.ResponseHeaders = x.responseHeaders

    /// Gets the response body stream.
    member x.ResponseBody = x.responseBody

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

/// OWIN App Delegate signature using F# Async.
type OwinApp = IDictionary<string, obj> -> Async<unit>

/// .NET language interop helpers
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OwinApp =
    /// Converts a F# Async-based OWIN App Delegate to a standard Func<_,Task> App Delegate.
    [<CompiledName("ToAppDelegate")>]
    let toAppDelegate (app: OwinApp) = Func<_,_>(fun d -> Async.StartAsTask (app d) :> Task)
