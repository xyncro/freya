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

/// An Environment dictionary to store OWIN request and response values.
type Environment =
    inherit Dictionary<string, obj>

    val private requestHeaders   : IDictionary<string, string[]>
    val private requestBody      : Stream
    val private responseHeaders  : IDictionary<string, string[]>
    val private responseBody     : Stream

    val mutable private disposed : bool

    /// Initializes a new Environment from an existing, valid, OWIN environment dictionary.
    new (dictionary: IDictionary<_,_>) =        
        {
            inherit Dictionary<string, obj>(dictionary, StringComparer.Ordinal)
            disposed = false
            requestHeaders = unbox dictionary.[Constants.requestHeaders]
            requestBody = unbox dictionary.[Constants.requestBody]
            responseHeaders = unbox dictionary.[Constants.responseHeaders]
            responseBody = unbox dictionary.[Constants.responseBody]
        }

    /// Initializes a new Environment from parameters, adding defaults for optional response parameters.
    new (requestMethod: string, requestScheme: string, requestPathBase: string, requestPath: string, requestQueryString: string, requestProtocol: string, requestHeaders, ?requestBody, ?responseHeaders, ?responseBody) as x =
        // TODO: Consider parsing the URI rather than requiring the pieces to be passed in explicitly.
        {
            inherit Dictionary<string, obj>(StringComparer.Ordinal)
            disposed = false
            requestHeaders = requestHeaders
            requestBody = defaultArg requestBody Stream.Null
            responseHeaders = defaultArg responseHeaders (new Dictionary<_,_>(HashIdentity.Structural) :> IDictionary<_,_>)
            responseBody = defaultArg responseBody (new MemoryStream() :> Stream)
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

    /// Gets a value with the specified key from the environment dictionary as the specified type 'a.
    static member inline Get<'a> (environment: IDictionary<string, obj>, key: string) =
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
    /// Returns an Environment from an IDictionary<string, obj>.
    /// If the dictionary is already an Environment, then the instance is cast and returned
    /// rather than mapped into a new instance.
    [<CompiledName("ToEnvironment")>]
    let toEnvironment (environment: IDictionary<string, obj>) =
        match environment with
        | :? Environment as e -> e
        | _ as d -> new Environment(d)
