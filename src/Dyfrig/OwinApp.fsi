//----------------------------------------------------------------------------
//
// Copyright (c) 2013-2014 Ryan Riley (@panesofglass) and Andrew Cherry (@kolektiv)
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
open FSharpx

/// OWIN environment dictionary
type OwinEnv = IDictionary<string, obj>

/// OWIN AppFunc signature using F# Async
type OwinApp = OwinEnv -> Async<unit>

/// OWIN AppFunc signature
type OwinAppFunc = Func<OwinEnv, Task>

/// OWIN monad functions and builder
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OwinMonad =
    /// OWIN monad signature
    type OwinMonad<'a> = OwinEnv -> Async<'a * OwinEnv>

    val fromAsync : f:Async<'a> -> OwinMonad<'a>

    val result : x:'a -> OwinMonad<'a>

    val bind : f:('a -> OwinMonad<'b>) -> m:OwinMonad<'a> -> OwinMonad<'b>

    val compose : m:('a -> OwinMonad<'b>) -> f:('b -> OwinMonad<'c>) -> x:'a -> OwinMonad<'c>

    val map : f:('a -> 'b) -> m:OwinMonad<'a> -> OwinMonad<'b>

    /// OWIN monad builder
    type OwinMonadBuilder =

        new : unit -> OwinMonadBuilder

        member Return : x:'a -> OwinMonad<'a>

        member ReturnFrom : f:OwinMonad<'a> -> OwinMonad<'a> 

        member Zero : unit -> OwinMonad<unit>

        member Bind : m:OwinMonad<'a> * k:('a -> OwinMonad<'b>) -> OwinMonad<'b>

        member Delay : f:(unit -> OwinMonad<'a>) -> OwinMonad<'a>

        member Combine : m1:OwinMonad<unit> * m2:OwinMonad<'a> -> OwinMonad<'a>

        member TryWith : body:OwinMonad<'a> * handler:(exn -> OwinMonad<'a>) -> OwinMonad<'a>

        member TryFinally : body:OwinMonad<'a> * handler:(unit -> unit) -> OwinMonad<'a>

        member Using<'a, 'b when 'a :> IDisposable> : resource:'a * body:('a -> OwinMonad<'b>) -> OwinMonad<'b>

        member While : guard:(unit -> bool) * body:OwinMonad<unit> -> OwinMonad<unit>

        member For : s:seq<'a> * body:('a -> OwinMonad<unit>) -> OwinMonad<unit>

    /// OWIN monad
    val owin : OwinMonadBuilder

    /// Get a value from the OwinEnv using a lens
    val get : l:Lens<OwinEnv, 'a> -> OwinMonad<'a>

    /// Set a value in the OwinEnv using a lens
    val set : l:Lens<OwinEnv, 'a> -> v:'a -> OwinMonad<unit>

    /// Update a value in the OwinEnv using a lens
    val update : l:Lens<OwinEnv, 'a> -> f:('a -> 'a) -> OwinMonad<unit>

/// OWIN operators
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Operators =
    open OwinMonad

    /// Left to right composition
    val inline (>>=) : m:OwinMonad<'a> -> f:('a -> OwinMonad<'b>) -> OwinMonad<'b>

    /// Right to left composition
    val inline (=<<) : f:('a -> OwinMonad<'b>) -> m:OwinMonad<'a> -> OwinMonad<'b>

    /// Left to right Kleisli composition
    val inline (>=>) : m:('a -> OwinMonad<'b>) -> f:('b -> OwinMonad<'c>) -> ('a -> OwinMonad<'c>)

    /// Right to left Kleisli composition
    val inline (<=<) : f:('b -> OwinMonad<'c>) -> m:('a -> OwinMonad<'b>) -> ('a -> OwinMonad<'c>)

    /// Map of f over m
    val inline (<!>) : f:('a -> 'b) -> m:OwinMonad<'a> -> OwinMonad<'b>

    /// Right to left set value using lens
    val inline (-->) : v:'a -> l:Lens<OwinEnv, 'a> -> OwinMonad<unit>

    /// Left to right set value using lens
    val inline (<--) : l:Lens<OwinEnv, 'a> -> v:'a -> OwinMonad<unit>

    /// Right to left update value using lens
    val inline (-!>) : f:('a -> 'a) -> l:Lens<OwinEnv, 'a> -> OwinMonad<unit>

    /// Left to right update value using lens
    val inline (<!-) : l:Lens<OwinEnv, 'a> -> f:('a -> 'a) -> OwinMonad<unit>

/// .NET language interop helpers
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OwinAppFunc =

    /// Converts a F# Async-based OWIN AppFunc to a standard Func<_,Task> AppFunc.
    [<CompiledName("FromOwinApp")>]
    val fromOwinApp : app:OwinApp -> OwinAppFunc

    /// Converts a F# OwinMonad<_> to a standard Func<_,Task> AppFunc.
    [<CompiledName("FromOwinMonad")>]
    val fromOwinMonad : monad:OwinMonad.OwinMonad<_> -> OwinAppFunc
