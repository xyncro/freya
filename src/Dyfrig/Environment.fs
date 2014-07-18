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

type OwinHeaders = IDictionary<string, string[]>

type Environment =
    inherit Dictionary<string, obj>

    val private requestHeaders   : OwinHeaders
    val private requestBody      : Stream
    val private responseHeaders  : OwinHeaders
    val private responseBody     : Stream
    val private callCancelled    : CancellationToken
    val private owinVersion      : string

    val mutable private disposed : bool

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

    static member inline Get<'a> (environment: OwinEnv, key: string) =
        if environment.ContainsKey(key) then
            Some(environment.[key] :?> 'a)
        else None

    member x.RequestMethod
        with get() : string = unbox x.[Constants.requestMethod]
        and set(v : string) = x.[Constants.requestMethod] <- v

    member x.RequestScheme
        with get() : string = unbox x.[Constants.requestScheme]
        and set(v : string) = x.[Constants.requestScheme] <- v

    member x.RequestPathBase
        with get() : string = unbox x.[Constants.requestPathBase]
        and set(v : string) = x.[Constants.requestPathBase] <- v

    member x.RequestPath
        with get() : string = unbox x.[Constants.requestPath]
        and set(v : string) = x.[Constants.requestPath] <- v

    member x.RequestQueryString
        with get() : string = unbox x.[Constants.requestQueryString]
        and set(v : string) = x.[Constants.requestQueryString] <- v

    member x.RequestProtocol
        with get() : string = unbox x.[Constants.requestProtocol]
        and set(v : string) = x.[Constants.requestProtocol] <- v
    
    member x.RequestHeaders = x.requestHeaders

    member env.GetBaseUri() =
        if env.RequestHeaders.ContainsKey("Host") then
            env.RequestScheme + "://" +
            (env.RequestHeaders.["Host"].[0]) +
            if String.IsNullOrEmpty env.RequestPathBase then "/" else env.RequestPathBase
            |> Some
        else None

    member env.GetRequestUri() =
        if env.RequestHeaders.ContainsKey("Host") then
            env.RequestScheme + "://" +
            (env.RequestHeaders.["Host"].[0]) +
            env.RequestPathBase +
            env.RequestPath +
            if String.IsNullOrEmpty env.RequestQueryString then "" else "?" + env.RequestQueryString
            |> Some
        else None
 
    member x.RequestBody = x.requestBody

    member x.ResponseStatusCode
        with get() : int =
            if x.ContainsKey(Constants.responseStatusCode) then
                unbox x.[Constants.responseStatusCode]
            else 200 // Default for HTTP 200 OK
        and set(v : int) = x.[Constants.responseStatusCode] <- v

    member x.ResponseReasonPhrase
        with get() : string =
            if x.ContainsKey(Constants.responseReasonPhrase) then
                unbox x.[Constants.responseReasonPhrase]
            elif x.ContainsKey(Constants.responseStatusCode) then
                unbox x.[Constants.responseStatusCode] |> enum<HttpStatusCode> |> string
            else "OK" // Default for HTTP 200 OK
        and set(v : string) = x.[Constants.responseReasonPhrase] <- v

    member x.ResponseProtocol
        with get() : string option =
            if x.ContainsKey(Constants.responseProtocol) then
                Some(unbox x.[Constants.responseProtocol])
            else None
        and set(v : string option) =
            match v with
            | Some v -> x.[Constants.responseProtocol] <- v
            | None -> x.Remove(Constants.responseProtocol) |> ignore

    member x.ResponseHeaders = x.responseHeaders

    member x.ResponseBody = x.responseBody

    member x.CallCancelled
        with get() : CancellationToken = unbox x.[Constants.callCancelled]
        and set(v : CancellationToken) = x.[Constants.callCancelled] <- v

    member x.OwinVersion = x.owinVersion

    member x.With(key: string, value: #obj) =
        let env = new Environment(x)
        env.[key] <- value
        env

    abstract Dispose : bool -> unit
    default x.Dispose(disposing) =
        if disposing then
            x.Values.OfType<IDisposable>()
            |> Seq.filter (fun x -> x <> Unchecked.defaultof<_>)
            |> Seq.iter (fun x -> try x.Dispose() with | _ -> ()) // TODO: Log any failed disposals.

    member x.Dispose() =
        if not x.disposed then
            GC.SuppressFinalize(x)
            x.Dispose(true)
            x.disposed <- true

    interface IDisposable with
        member x.Dispose() = x.Dispose()


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Environment =
    [<CompiledName("ToEnvironment")>]
    let toEnvironment (environment: OwinEnv) =
        match environment with
        | :? Environment as e -> e
        | _ as d -> new Environment(d)
