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

    let bind (f: 'TIn -> OwinRailway<'TOut, 'TFailure>) (input: OwinRailway<'TIn, 'TFailure>) = async {
        let! inbound = input
        match inbound with
        | Choice1Of2 success -> return! f success
        | Choice2Of2 e -> return Choice2Of2 e
    }

    let map (f: 'TIn -> 'TOut) (input: OwinRailway<'TIn, 'TFailure>) =
        input |> bind (f >> Choice1Of2 >> async.Return)

    let mapAsync (f: 'TIn -> Async<'TOut>) (input: OwinRailway<'TIn, 'TFailure>) = async {
        let! inbound = input
        match inbound with
        | Choice1Of2 success ->
            let! result = f success
            return Choice1Of2 result
        | Choice2Of2 e -> return Choice2Of2 e
    }

    let private copyStream bufferSize =
        let buffer = Array.zeroCreate bufferSize
        let rec moveTo (outs: System.IO.Stream) (ins: System.IO.Stream) = async {
            let! bytes = ins.AsyncRead(buffer)
            if bytes > 0 then
                do! outs.AsyncWrite(buffer, 0, bytes)
                return! moveTo outs ins
        }
        moveTo

    type System.IO.Stream with
        member x.AsyncCopyTo (out: System.IO.Stream, ?bufferSize) = async {   
            let bufferSize = defaultArg bufferSize 1024
            let copyTo = copyStream bufferSize
            return! copyTo out x
        }

    /// Converts a F# Async-based railway-oriented OWIN AppFunc to a standard Func<_, Task> AppFunc.
    [<CompiledName("FromRailway")>]
    let fromRailway (exceptionHandler: Environment -> exn -> Environment) (app: OwinEnv -> OwinRailway<Environment, exn>) =
        let handler env = async {
            let env = Environment.toEnvironment env
            let! result = app env
            let env' =
                match result with
                | Choice1Of2 env' -> env'
                | Choice2Of2 e -> exceptionHandler env e

            // If the handler mutated the environment, no more work is necessary.
            if obj.ReferenceEquals(env, env') then () else
            // Otherwise, copy the last dictionary back onto the original.
            for KeyValue(key, value) in env' do
                // TODO: What elements might we not want to copy? Are all safe to copy?
                if env.ContainsKey(key) && env.[key] <> value then
                    match value with
                    | :? System.IO.Stream as stream ->
                        let out = unbox<System.IO.Stream> env.[key]
                        // TODO: asynchronously copy to the out stream
                        do! stream.AsyncCopyTo(out)
                    | _ -> env.[key] <- value
        }
        OwinAppFunc(fun env -> handler env |> Async.StartAsTask :> System.Threading.Tasks.Task)
