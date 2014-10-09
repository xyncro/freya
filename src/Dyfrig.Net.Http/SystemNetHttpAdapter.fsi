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
namespace Dyfrig.Net.Http

/// `Stream` decorator to prevent `System.Net.Http` types from closing the provided `Stream` from the `Environment`.
[<Sealed; Class>]
type ProtectedStream = 
    inherit System.IO.Stream

/// Helper functions for working with an OWIN environment dictionary
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SystemNetHttpAdapter = 
    open Dyfrig
    
    /// Converts the `OwinEnv` into an `HttpRequestMessage`.
    [<CompiledName("ToHttpRequestMesage")>]
    val toHttpRequestMessage : environment:OwinEnv -> System.Net.Http.HttpRequestMessage option
    /// Invokes an `HttpResponseMessage` in an OWIN handler.
    [<CompiledName("InvokeHttpResponseMessage")>]
    val invokeHttpResponseMessage : environment:OwinEnv -> response:System.Net.Http.HttpResponseMessage -> Async<unit>
    /// Adapts a function of type `HttpRequestMessage -> Async<HttpResponseMessage>` to an OWIN handler.
    [<CompiledName("FromAsyncSystemNetHttp")>]
    val fromAsyncSystemNetHttp : handler:(System.Net.Http.HttpRequestMessage -> Async<System.Net.Http.HttpResponseMessage>)
         -> OwinAppFunc
    /// Adapts a function of type `HttpRequestMessage -> Task<HttpResponseMessage>` to an OWIN handler.
    [<CompiledName("FromSystemNetHttp")>]
    val fromSystemNetHttp : handler:(System.Net.Http.HttpRequestMessage -> System.Threading.Tasks.Task<System.Net.Http.HttpResponseMessage>)
         -> OwinAppFunc
    /// Maps an `HttpResponseMessage` to an `Environment`.
    [<CompiledName("MapResponseToEnvironment")>]
    val mapResponseToEnvironment : environment:OwinEnv -> response:System.Net.Http.HttpResponseMessage -> Async<OwinEnv>
