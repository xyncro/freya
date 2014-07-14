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
type OwinRailway = OwinEnv -> Async<Choice<OwinEnv, exn>>

/// OWIN railway helper functions
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OwinRailway =

    /// Converts a F# Async-based railway-oriented OWIN AppFunc to a standard Func<_, Task> AppFunc.
    [<CompiledName("FromRailway")>]
    let fromRailway (exceptionHandler: OwinEnv -> exn -> Async<unit>) (app: OwinRailway) =
        let handler env = async {
            let env = Environment.toEnvironment env
            let! result = app env
            match result with
            | Choice1Of2 env' ->
                // NOTE: The following will be especially important once `Environment` is immutable.
                // Copy the last dictionary back onto the original.
                for KeyValue(key, value) in env' do
                    // TODO: What elements might we not want to copy? Are all safe to copy?
                    if env.ContainsKey(key) && env.[key] <> value
                    then env.[key] <- value
                return ()
            | Choice2Of2 e ->
                do! exceptionHandler env e }
        OwinAppFunc(fun env -> handler env |> Async.StartAsTask :> System.Threading.Tasks.Task)
