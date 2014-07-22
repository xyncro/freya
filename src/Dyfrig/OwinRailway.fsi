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

/// OWIN AppFunc suitable for chaining composable functions
type OwinRailway<'TSuccess, 'TFailure> = Async<Choice<'TSuccess, 'TFailure>>

/// OWIN railway helper functions
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OwinRailway =

    /// Compose a two-track input with a one track function returning a two-track result.
    val bind : f:('TIn -> OwinRailway<'TOut, 'TFailure>) -> input:OwinRailway<'TIn, 'TFailure> -> OwinRailway<'TOut, 'TFailure>

    /// Combine a two-track input with a projection on the success track and ignore the failure track.
    val map : f:('TIn -> 'TOut) -> input:OwinRailway<'TIn, 'TFailure> -> OwinRailway<'TOut, 'TFailure>

    /// Combine a two-track input with an asynchronous projection on the success track and ignore the failure track.
    val mapAsync : f:('TIn -> Async<'TOut>) -> input:OwinRailway<'TIn, 'TFailure> -> OwinRailway<'TOut, 'TFailure>

    /// Extensions to the `System.IO.Stream` type.
    type System.IO.Stream with
        /// Asynchronously copy from the current `System.IO.Stream` to another `System.IO.Stream` using a fixed-size buffer.
        member AsyncCopyTo : out:System.IO.Stream * ?bufferSize:int -> Async<unit>   

    /// Converts a F# Async-based railway-oriented OWIN AppFunc to a standard Func<_, Task> AppFunc.
    [<CompiledName("FromRailway")>]
    val fromRailway : exceptionHandler:(Environment -> exn -> Environment) -> app:(OwinEnv -> OwinRailway<Environment, exn>) -> OwinAppFunc
