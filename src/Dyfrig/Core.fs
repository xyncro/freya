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

open System
open System.Collections.Generic
open System.Threading.Tasks
open Aether
open FSharpx
open FSharpx.Async

/// OWIN environment dictionary
type OwinEnv = IDictionary<string, obj>

/// OWIN AppFunc signature using F# Async
type OwinApp = OwinEnv -> Async<unit>

/// OWIN AppFunc signature
type OwinAppFunc = Func<OwinEnv, Task>

/// OWIN monad implementation
[<AutoOpen>]
module Monad =

    /// OWIN monad signature
    type OwinMonad<'T> = 
        OwinEnv -> Async<'T * OwinEnv>

    /// OWIN monad builder
    type OwinMonadBuilder () =

        member x.Return t : OwinMonad<_> = 
            fun e -> async.Return (t, e)

        member x.ReturnFrom f : OwinMonad<_> = 
            f

        member x.Zero () : OwinMonad<unit> = 
            x.Return ()

        member x.Bind (m: OwinMonad<_>, k: _ -> OwinMonad<_>) : OwinMonad<_> =
            fun e -> m e >>= fun (result, e) -> (k result) e

        member x.Delay (f: unit -> OwinMonad<_>) : OwinMonad<'T> = 
            x.Bind (x.Return (), f)

        member x.Combine (m1: OwinMonad<_>, m2: OwinMonad<_>) : OwinMonad<_> = 
            x.Bind (m1, fun () -> m2)

        member x.TryWith (body: OwinMonad<_>, handler: exn -> OwinMonad<_>) : OwinMonad<_> =
            fun e -> try body e with ex -> handler ex e

        member x.TryFinally (body: OwinMonad<_>, handler) : OwinMonad<_> =
            fun e -> try body e finally handler ()

        member x.Using (resource: #IDisposable, body: _ -> OwinMonad<_>) : OwinMonad<_> =
            x.TryFinally (body resource, fun () ->
                match box resource with
                | null -> ()
                | _ -> resource.Dispose ())

        member x.While (guard, body: OwinMonad<_>) : OwinMonad<unit> =
            match guard () with
            | true -> x.Bind (body, fun () -> x.While (guard, body))
            | _ -> x.Return ()

        member x.For (s: seq<_>, body: _ -> OwinMonad<_>) : OwinMonad<unit> =
            x.Using (s.GetEnumerator (),
                (fun enum ->
                    x.While (enum.MoveNext, x.Delay (fun () ->
                        body enum.Current))))

    /// OWIN monad
    let owin = OwinMonadBuilder ()

    /// Get a value from the OwinEnv using an Aether typed lens
    let getLM l : OwinMonad<_> = 
        fun s -> async { return getL l s, s }

    let getPLM l : OwinMonad<_> = 
        fun s -> async { return getPL l s, s }

    /// Set a value in the OwinEnv using an Aether typed lens
    let setLM l v : OwinMonad<unit> =
        fun s -> async { return (), setL l v s }

    let setPLM l v : OwinMonad<unit> =
        fun s -> async { return (), setPL l v s }

    /// Modify a value in the OwinEnv using an Aether typed lens
    let modLM l f : OwinMonad<unit> =
        fun s -> async { return (), modL l f s }

    let modPLM l f : OwinMonad<unit> =
        fun s -> async { return (), modPL l f s }

/// OWIN monad functions
module Owin =

    let async f : OwinMonad<_> =
        fun s -> async { return! flip tuple2 s <!> f }

    let compose m f =
        fun x ->
            owin {
                let! v = m x
                return! f v }

    let composeSeq m f =
        owin {
            let! v = m
            return! f v }

    let map f m =
        owin {
            let! v = m
            return f v }

/// OWIN operators
module Operators =

    /// Left to right composition
    let inline (>>=) m f = Owin.composeSeq m f

    /// Right to left composition
    let inline (=<<) f m = Owin.composeSeq m f

    /// Left to right Kleisli composition
    let inline (>=>) m f = Owin.compose m f

    /// Right to left Kleisli composition
    let inline (<=<) f m = Owin.compose m f

    /// Map of f over m
    let inline (<!>) f m = Owin.map f m

/// .NET language interop helpers
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OwinAppFunc =

    /// Converts a F# Async-based OWIN AppFunc to a standard Func<_,Task> AppFunc.
    [<CompiledName("FromOwinApp")>]
    let fromOwinApp (app: OwinApp) =
        OwinAppFunc (fun env -> Async.StartAsTask (app env) :> Task)

    /// Converts a F# OwinMonad<_> to a standard Func<_,Task> AppFunc.
    [<CompiledName("FromOwinMonad")>]
    let fromOwinMonad (monad: OwinMonad<_>) =
        fromOwinApp (fun e -> async { do! monad e |> Async.Ignore })
