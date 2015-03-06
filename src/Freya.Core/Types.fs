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

namespace Freya.Core

open System
open System.Collections.Generic

(* Environment *)

/// Type alias for <see cref="IDictionary<T1, T2>" /> using <see cref="String" /> for keys and containing boxed values.
type FreyaEnvironment =
    IDictionary<string, obj>

(* State *)

/// A state value to be threaded through Freya computations,
/// including the <see cref="CoreEnvironment" /> and <see cref="CoreMetaState" />
type FreyaState =
    { Environment: FreyaEnvironment
      Meta: FreyaMetaState }

    static member internal EnvironmentLens =
        (fun x -> x.Environment), 
        (fun e x -> { x with Environment = e })

    static member internal MetaLens =
        (fun x -> x.Meta), 
        (fun m x -> { x with Meta = m })

/// <summary>
/// A state value representing Core computations' memoized values.
/// </summary>
/// <remarks>
/// This state value allows Freya to avoid polluting the <see cref="CoreEnvironment" />
/// with Freya-specific concerns.
/// </remarks>
and FreyaMetaState =
    { Memos: Map<Guid, obj> }

    static member internal MemosLens =
        (fun x -> x.Memos),
        (fun m x -> { x with Memos = m })

(* Computation Expression *)

/// <summary>
/// An <see cref="Async{T}" /> state computation type using <see cref="CoreState" />
/// as the state value and a generic type for the computed result.
/// </summary>
/// <remarks>
/// <see cref="Core{T}" /> represents an asynchronous state monad and forms the core all composition within Freya.
/// Note that the definition does not use a generic value for state but specifies <see cref="CoreState" />
/// explicitly as the state value. Further, this definition does not make use of a wrapped value but simply
/// provides an alias for the function definition. Both offer slight performance optimizations and allow Freya
/// to require additional dependencies.
/// </remarks>
type Freya<'T> =
    FreyaState -> Async<'T * FreyaState>