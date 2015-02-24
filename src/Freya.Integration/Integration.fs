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
//
//----------------------------------------------------------------------------

module Freya.Integration

open System
open System.Threading.Tasks
open Freya.Core
open Freya.Pipeline

(* OWIN Types *)

/// Type alias for the OWIN MidFunc signature.
type OwinMidFunc =
    Func<OwinAppFunc, OwinAppFunc>

(* OWIN Conversion *)

/// Provides transformation functions for converting to/from OWIN middlewares from/to Freya Pipelines.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module OwinMidFunc =

    let private any (list: Async<'T>[])=
        let tcs = new TaskCompletionSource<'T>()

        list |> Array.map (fun wf->Async.Start (async{
                    let! res=wf
                    tcs.TrySetResult (res) |> ignore
                }))
             |> ignore

        Async.AwaitTask tcs.Task
    
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
        // TODO: find a better way than using TaskCompletionSource instances
        let signalCh = TaskCompletionSource<bool>()
        let continueCh = TaskCompletionSource<unit>()
        let midFuncCh = ref Unchecked.defaultof<Task<bool>>
        let nextSignal =
            OwinAppFunc(fun _ ->
                signalCh.SetResult true
                continueCh.Task :> Task)
        let before s =
            async {
                let! token = Async.CancellationToken
                // Apply and mutate the OwinEnvironment asynchronously.
                midFuncCh := midFunc.Invoke(nextSignal).Invoke(s.Environment).ContinueWith((fun _ -> false), token)
                // If haltCh completes first, nextWasRun will be false, and nothing further is necessary.
                // Otherwise, we'll continue on and call back into the midFunc in the after pipe.
                let! task = Async.AwaitTask <| Task.WhenAny ([| signalCh.Task; !midFuncCh |])
                // If the Task is in a faulted state, raise an exception.
                if task.Status = TaskStatus.Faulted then
                    raise task.Exception
                // Return Next or Halt depending on the Task.Status
                if task.Status = TaskStatus.RanToCompletion && task.Result then
                    // Return the Next flag and the mutated FreyaState.
                    return Next, s
                // NOTE: if we Halt here, after will never be called. This will break MidFunc expectations, I think.
                else return Halt, s }
        // TODO: How do we ensure that after is always called? I'm not sure what will happen if the Freya pipeline Halts before calling after.
        let after s =
            async {
                // Set the continueCh result to complete the signal Task and allow the middleware to complete.
                continueCh.SetResult ()
                // Await the completion of the midFuncCh to know when the midFunc completes
                let task = !midFuncCh
                let! _ = Async.AwaitTask task
                // If the Task is in a faulted state, raise an exception.
                if task.Status = TaskStatus.Faulted then
                    raise task.Exception
                // Return Next or Halt depending on the Task.Status
                if task.Status = TaskStatus.RanToCompletion then
                    return Next, s
                else return Halt, s }
        before, after
