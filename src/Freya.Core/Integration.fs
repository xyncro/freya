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

[<AutoOpen>]
module Freya.Core.Integration

open System
open System.Threading.Tasks

(* OWIN Types *)

/// Type alias of <see cref="FreyaEnvironment" /> in terms of OWIN.
type OwinEnvironment =
    FreyaEnvironment

/// Type alias for the F# equivalent of the OWIN AppFunc signature.
type OwinApp = 
    OwinEnvironment -> Async<unit>

/// Type alias for the OWIN AppFunc signature.
type OwinAppFunc = 
    Func<OwinEnvironment, Task>

(* OWIN Conversion *)

/// Provides transformation functions for converting to/from OWIN from/to Freya.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module OwinAppFunc =
    open Freya.Core.Functions

    /// Converts a <see cref="Freya{T}" /> computation to an <see cref="OwinAppFunc" />.
    [<CompiledName ("FromFreya")>]
    let fromFreya (freya: Freya<_>) =
        OwinAppFunc (fun e ->
            async {
                do! freya { Environment = e
                            Meta = { Memos = Map.empty } } |> Async.Ignore }
            |> Async.StartAsTask :> Task)
    
    /// Converts an <see cref="OwinAppFunc" /> to a <see cref="Freya{T}" /> computation
    /// to allow use of standard OWIN components within Freya.
    /// NOTE: EXPERIMENTAL
    [<CompiledName ("ToFreya")>]
    let toFreya (app: OwinAppFunc) : Freya<unit> =
        // TODO: Can another, existing operator handle this scenario better?
        fun s -> async {
            // Retrieve the OwinEnvironment
            let { Environment = e; Meta = _ } = s
            let! token = Async.CancellationToken
            // Apply and mutate the OwinEnvironment asynchronously
            let! _ = app.Invoke(e).ContinueWith<unit>((fun t -> ()), token) |> Async.AwaitTask
            // Return the result as a unit value and the mutated FreyaState
            // TODO: should the current value be retrieved and threaded through, or is it more appropriate to return unit?
            return (), s }
