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
    
    /// Converts a Freya.Pipeline to an OWIN MidFunc run before executing the next OwinAppFunc.
    [<CompiledName("FromFreya")>]
    let ofFreya (pipeline: Freya<FreyaPipelineChoice>) =
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
    
    /// Converts a Freya.Pipeline to an OWIN MidFunc run after executing the next OwinAppFunc.
    [<CompiledName("FromFreyaAfter")>]
    let ofFreyaAfter (pipeline: Freya<FreyaPipelineChoice>) =
        OwinMidFunc(fun next ->
            let app e =
                async {
                    let task = next.Invoke e
                    do! task.ContinueWith<unit>(fun _ -> ()) |> Async.AwaitTask
                    // Check the result. If the Task is complete run the after pipeline.
                    if (task.Status = TaskStatus.RanToCompletion) then
                        // Convert to FreyaState
                        let s = { Environment = e
                                  Meta = { Memos = Map.empty } }
                        // Execute the pipeline
                        do! pipeline s |> Async.Ignore
                    else return () }
                |> Async.StartAsTask :> Task
            OwinAppFunc app)

    /// Converts a Freya.Pipeline to an OWIN MidFunc.
    [<CompiledName("FromFreyaWrapped")>]
    let ofFreyaWrapped (before: Freya<FreyaPipelineChoice>) (after: Freya<FreyaPipelineChoice>) =
        OwinMidFunc(fun next ->
            let app e =
                // Convert to FreyaState
                let s = { Environment = e
                          Meta = { Memos = Map.empty } }
                async {
                    // Execute the before pipeline
                    let! choice, s' = before s
                    match choice with
                    // Execute the next OwinAppFunc
                    | Next ->
                        let task = next.Invoke(s'.Environment)
                        do! task.ContinueWith<unit>(fun _ -> ()) |> Async.AwaitTask
                        // Check the result. If the Task is complete run the after pipeline.
                        if (task.Status = TaskStatus.RanToCompletion) then
                            // Execute the after pipeline
                            do! after s' |> Async.Ignore
                        else return ()
                    // Complete the Task
                    | Halt -> return () }
                |> Async.StartAsTask :> Task
            OwinAppFunc app)

    /// Splits a MidFunc into a before and after Freya.Pipeline.
    [<CompiledName("SplitIntoFreya")>]
    let splitIntoFreya (midFunc: OwinMidFunc) : Freya<FreyaPipelineChoice> * Freya<FreyaPipelineChoice> =
        let nextWasRun = ref false
        let resumed = new Event<unit>()
        let nextSignal =
            OwinAppFunc(fun _ ->
                async {
                    nextWasRun := true
                    // Pause while the rest of the pipeline runs.
                    do! resumed.Publish |> Async.AwaitEvent
                    // Complete the Task.
                    return ()
                } |> Async.StartAsTask :> Task)
        let before s =
            async {
                let! token = Async.CancellationToken
                // Apply and mutate the OwinEnvironment asynchronously.
                do! Async.AwaitTask <| midFunc.Invoke(nextSignal).Invoke(s.Environment).ContinueWith<unit>((fun _ -> ()), token)
                if !nextWasRun then
                    // Return the result as a unit value and the mutated FreyaState.
                    return Next, s
                else return Halt, s }
        let after s =
            async {
                // Trigger the resumed event to allow the MidFunc to continue processing.
                resumed.Trigger ()
                // NOTE: This should be ignored; it is here purely to support the correct signature.
                return Next, s }
        before, after
