//----------------------------------------------------------------------------
//
// Copyright (c) 2014
//
//    Ryan Riley (@panesofglass) and Andrew Cherry (@kolektiv)
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

module Freya.Integration

open System
open System.Threading.Tasks
open Freya.Core

(* OWIN Conversion *)

/// Provides transformation functions for converting to/from OWIN from/to Freya.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module OwinAppFunc =
    
    /// Converts an <see cref="OwinAppFunc" /> to a <see cref="Freya{T}" /> computation
    /// to allow use of standard OWIN components within Freya.
    /// NOTE: EXPERIMENTAL
    [<CompiledName ("ToFreya")>]
    let toFreya (app: OwinAppFunc) : Freya<unit> =
        fun s -> async {
            let! token = Async.CancellationToken
            // Apply and mutate the OwinEnvironment asynchronously
            do! Async.AwaitTask <| app.Invoke(s.Environment).ContinueWith<unit>((fun _ -> ()), token)
            // Return the result as a unit value and the mutated FreyaState
            return (), s }

open Freya.Pipeline

/// Type alias for the OWIN MidFunc signature.
type OwinMidFunc =
    Func<OwinAppFunc, OwinAppFunc>

/// Provides transformation functions for converting to/from OWIN middlewares from/to Freya Pipelines.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module OwinMidFunc =
    
    /// Converts a Freya.Pipeline to an OWIN MidFunc.
    [<CompiledName("FromFreya")>]
    let fromFreya (pipeline: Freya<FreyaPipelineChoice>) =
        OwinMidFunc(fun next ->
            let app e =
                // Convert to FreyaState
                let s = { Environment = e
                          Meta = { Memos = Map.empty } }
                async {
                    // Execute the pipeline
                    let! choice, s' = pipeline s
                    match choice with
                    // Execute the next OwinAppFunc
                    | Next ->
                        return! next.Invoke(s'.Environment).ContinueWith<unit>(fun _ -> ()) |> Async.AwaitTask
                    // Complete the Task
                    | Halt -> return () }
                |> Async.StartAsTask :> Task
            OwinAppFunc app)

    // NOTES:
    // * Incorporating the MidFunc will require evaluating the state of the Task after running the middleware.
    // * If the Task is completed, return Halt.
    // * If the Task is not completed, return Next.
    // * How do you spilt the MidFunc? A MidFunc can take actions on the way in and on the way out.
//    [<CompiledName("ToFreya")>]
//    let toFreya (midFunc: OwinMidFunc) : Freya<FreyaPipelineChoice> =
//        fun s -> async {
//            let! token = Async.CancellationToken
//            // Apply and mutate the OwinEnvironment asynchronously
//            do! Async.AwaitTask <| midFunc.Invoke(s.Environment).ContinueWith<unit>((fun _ -> ()), token)
//            // Return the result as a unit value and the mutated FreyaState
//            return Next, s
//        }
