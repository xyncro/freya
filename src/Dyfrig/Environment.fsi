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

/// OWIN headers dictionary
type OwinHeaders = System.Collections.Generic.IDictionary<string, string[]>

/// An Environment dictionary to store OWIN request and response values.
[<Class>]
type Environment =
    inherit System.Collections.Generic.Dictionary<string, obj>
    interface System.IDisposable

    /// Initializes a new Environment from an existing, valid, OWIN environment dictionary.
    new : dictionary:OwinEnv -> Environment

    /// Initializes a new Environment from parameters, adding defaults for optional response parameters.
    new : requestMethod:string * requestScheme:string * requestPathBase:string * requestPath:string * requestQueryString:string * requestProtocol:string * requestHeaders:OwinHeaders * ?requestBody:System.IO.Stream * ?responseHeaders:OwinHeaders * ?responseBody:System.IO.Stream * ?callCancelled:System.Threading.CancellationToken -> Environment

    /// Gets a value with the specified key from the environment dictionary as the specified type 'a.
    static member inline Get<'a> : environment:OwinEnv * key: string -> 'a option

    /// Gets or sets the HTTP method used in the current request.
    member RequestMethod : string with get, set

    /// Gets or sets the scheme (e.g. "http" or "https") for the current request.
    member RequestScheme : string with get, set

    /// Gets or sets the path corresponding to the "root" of the application.
    member RequestPathBase : string with get, set

    /// Gets or sets the path relative to the "root" of the application.
    member RequestPath : string with get, set

    /// Gets or sets the query string from the request URI.
    member RequestQueryString : string with get, set

    /// Gets or sets the HTTP protocol version for the request.
    member RequestProtocol : string with get, set
    
    /// Gets the request headers dictionary for the current request.
    member RequestHeaders : OwinHeaders with get

    /// Reconstructs the base request URI from the component parts.
    member GetBaseUri : unit -> string option

    /// Reconstructs the request URI from the component parts.
    member GetRequestUri : unit -> string option
 
    /// Gets the request body for the current request.
    member RequestBody : System.IO.Stream with get

    /// Gets or sets the unique string representing the request.
    member RequestId : string option with get, set

    /// Gets or sets the `IPrincipal` instance representing the requesting user.
    /// In a .NET 4.5 project, this MUST be a `ClaimsPrincipal`.
    member RequestUser : System.Security.Principal.IPrincipal with get, set

    /// Gets or sets the response status code for the current request.
    member ResponseStatusCode : int with get, set

    /// Gets the response reason phrase for the current request.
    member ResponseReasonPhrase : string with get, set

    /// Gets the response status code for the current request.
    member ResponseProtocol : string option with get, set

    /// Gets the response headers dictionary for the current response.
    member ResponseHeaders : OwinHeaders with get

    /// Gets the response body stream.
    member ResponseBody : System.IO.Stream with get

    /// Gets or sets the `CancellationToken` indicating whether the request has been cancelled.
    member CallCancelled : System.Threading.CancellationToken with get, set

    /// Gets the current OWIN version.
    member OwinVersion : string with get

    /// Immutable update to the `Environment` returning a new copy of the `Environment`
    /// with the new or updated `key * value` pair.
    member With : key:string * value:#obj -> Environment

    /// Overridable disposal implementation for this instance.
    abstract Dispose : bool -> unit

    /// Disposes this instance.
    member Dispose : unit -> unit

/// Helper functions for working with an OWIN environment dictionary
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Environment =
    /// Returns an Environment from an `OwinEnv`.
    /// If the dictionary is already an Environment, then the instance is cast and returned
    /// rather than mapped into a new instance.
    [<CompiledName("ToEnvironment")>]
    val toEnvironment : environment:OwinEnv -> Environment
