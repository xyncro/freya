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
    open FSharpx.Async

    /// OWIN monad signature
    type OwinMonad<'T> = OwinEnv -> Async<'T * OwinEnv>

    let fromAsync f : OwinMonad<_> =
        fun s -> async { return! flip tuple2 s <!> f }

    let result x : OwinMonad<_> =
        fun e -> async.Return (x, e)

    let bind (f: 'a -> OwinMonad<'b>) (m: OwinMonad<'a>) : OwinMonad<'b> =
        fun e -> m e >>= fun (v, e) -> (f v) e

    let compose m f x : OwinMonad<_> =
        m x |> bind (fun v -> f v)

    let map f m : OwinMonad<_> =
        m |> bind (fun v -> result (f v))

    /// OWIN monad builder
    type OwinMonadBuilder () =

        member x.Return t : OwinMonad<_> = 
            fun e -> async.Return (t, e)

        member x.ReturnFrom f: OwinMonad<_> =
            f

        member x.Zero () =
            x.Return ()

        member x.Bind (m, k) : OwinMonad<_> =
            bind k m

        member x.Delay f =
            x.Bind (x.Return (), f)

        member x.Combine (m1, m2) =
            x.Bind (m1, fun () -> m2)

        member x.TryWith (body, handler) : OwinMonad<_> =
            fun e -> try body e with ex -> handler ex e

        member x.TryFinally (body, handler) : OwinMonad<_> =
            fun e -> try body e finally handler ()

        member x.Using (resource: #IDisposable, body) : OwinMonad<_> =
            x.TryFinally (body resource, fun () ->
                match box resource with
                | null -> ()
                | _ -> resource.Dispose ())

        member x.While (guard, body) : OwinMonad<unit> =
            match guard () with
            | true -> x.Bind (body, fun () -> x.While (guard, body))
            | _ -> x.Return ()

        member x.For (s: seq<_>, body) : OwinMonad<unit> =
            x.Using (s.GetEnumerator (),
                (fun enum ->
                    x.While (enum.MoveNext, x.Delay (fun () ->
                        body enum.Current))))

    /// OWIN monad
    let owin = OwinMonadBuilder ()

    /// Get a value from the OwinEnv using a lens
    let get l : OwinMonad<_> = 
        fun s -> async { return Lens.get s l, s }

    /// Set a value in the OwinEnv using a lens
    let set l v : OwinMonad<unit> =
        fun s -> async { return (), Lens.set v s l }

    /// Update a value in the OwinEnv using a lens
    let update l f : OwinMonad<unit> =
        fun s -> async { return (), Lens.update f l s }

/// OWIN operators
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Operators =
    open OwinMonad

    /// Left to right composition
    let inline (>>=) m f = bind f m

    /// Right to left composition
    let inline (=<<) f m = bind f m

    /// Left to right Kleisli composition
    let inline (>=>) m f = compose m f

    /// Right to left Kleisli composition
    let inline (<=<) f m = compose m f

    /// Map of f over m
    let inline (<!>) f m = map f m

    /// Right to left set value using lens
    let inline (-->) v l = set l v

    /// Left to right set value using lens
    let inline (<--) l v = set l v

    /// Right to left update value using lens
    let inline (-!>) f l = update l f

    /// Left to right update value using lens
    let inline (<!-) l f = update l f

/// .NET language interop helpers
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OwinAppFunc =

    /// Converts a F# Async-based OWIN AppFunc to a standard Func<_,Task> AppFunc.
    [<CompiledName("FromOwinApp")>]
    let fromOwinApp (app: OwinApp) =
        OwinAppFunc (fun env -> Async.StartAsTask (app env) :> Task)

    /// Converts a F# OwinMonad<_> to a standard Func<_,Task> AppFunc.
    [<CompiledName("FromOwinMonad")>]
    let fromOwinMonad (monad: OwinMonad.OwinMonad<_>) =
        fromOwinApp (fun e -> async { do! monad e |> Async.Ignore })
