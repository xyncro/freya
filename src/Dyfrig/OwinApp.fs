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
open System.Threading.Tasks

/// OWIN environment dictionary
type OwinEnv = IDictionary<string, obj>

/// OWIN AppFunc signature using F# Async
type OwinApp = OwinEnv -> Async<unit>

/// OWIN AppFunc signature
type OwinAppFunc = Func<OwinEnv, Task>

/// .NET language interop helpers
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OwinApp =
    /// Converts a F# Async-based OWIN AppFunc to a standard Func<_,Task> AppFunc.
    [<CompiledName("ToAppFunc")>]
    let toAppFunc (app: OwinApp) =
        OwinAppFunc(fun env -> Async.StartAsTask (app env) :> Task)
