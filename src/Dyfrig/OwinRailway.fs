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

    let bind f input = async {
        let! inbound = input
        match inbound with
        | Choice1Of2 success -> return! f success
        | Choice2Of2 e -> return Choice2Of2 e
    }

    let map f input =
        input |> bind (f >> Choice1Of2 >> async.Return)

    let mapAsync f input = async {
        let! inbound = input
        match inbound with
        | Choice1Of2 success ->
            let! result = f success
            return Choice1Of2 result
        | Choice2Of2 e -> return Choice2Of2 e
    }

    /// Converts a F# Async-based railway-oriented OWIN AppFunc to a standard Func<_, Task> AppFunc.
    [<CompiledName("FromRailway")>]
    let fromRailway exceptionHandler (app: OwinEnv -> OwinRailway<Environment, exn>) =
        let handler env = async {
            let env = Environment.toEnvironment env
            let! result = app env
            let env' =
                match result with
                | Choice1Of2 env' -> env'
                | Choice2Of2 e -> exceptionHandler env e
            do! env' |> Environment.flush env }
        OwinAppFunc(fun env -> handler env |> Async.StartAsTask :> System.Threading.Tasks.Task)
