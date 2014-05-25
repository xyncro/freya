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
open System.Threading.Tasks
open Microsoft.FSharp.Core

/// OWIN App Delegate signature using F# Async.
type OwinApp = IDictionary<string, obj> -> Async<unit>

/// .NET language interop helpers
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OwinApp =
    /// Converts a F# Async-based OWIN App Delegate to a standard Func<_,Task> App Delegate.
    [<CompiledName("ToAppDelegate")>]
    let toAppDelegate (app: OwinApp) = Func<_,_>(fun d -> Async.StartAsTask (app d) :> Task)
