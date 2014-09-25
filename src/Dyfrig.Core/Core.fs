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

namespace Dyfrig.Core

open System
open System.Collections.Generic
open System.Threading.Tasks


/// OWIN environment dictionary
type OwinEnv = 
    IDictionary<string, obj>

/// OWIN AppFunc signature using F# Async
type OwinApp = 
    OwinEnv -> Async<unit>

/// OWIN AppFunc signature
type OwinAppFunc = 
    Func<OwinEnv, Task>


/// OWIN monad implementation
[<AutoOpen>]
module Monad =

    type OwinMonad<'T> = 
        OwinEnv -> Async<'T * OwinEnv>

    type OwinMonadBuilder () =

        member __.Return (t) : OwinMonad<'T> = 
            fun env -> 
                async.Return (t, env)
    
        member __.ReturnFrom (m: OwinMonad<'T>) = 
            m
    
        member __.Bind (m1: OwinMonad<'T>, m2: 'T -> OwinMonad<'U>) : OwinMonad<'U> = 
            fun s -> 
                async { 
                    let! r, s = m1 s
                    return! (m2 r) s }
    
        member this.Zero () = 
            this.Return ()
    
        member this.Combine (m1: OwinMonad<unit>, m2: OwinMonad<'T>) : OwinMonad<'T> = 
            this.Bind (m1, fun () -> m2)
    
        member __.TryWith (m: OwinMonad<'T>, handler: exn -> OwinMonad<'T>) : OwinMonad<'T> =
            fun env ->
                 try m env
                 with e -> (handler e) env
    
        member __.TryFinally (m: OwinMonad<'T>, compensation) : OwinMonad<'T> =
            fun env -> 
                try m env
                finally compensation()
    
        member this.Using (res: #IDisposable, body) =
            this.TryFinally (body res, (fun () -> 
                match res with 
                | null -> () 
                | disp -> disp.Dispose()))
    
        member this.Delay (f) = 
            this.Bind (this.Return (), f)
    
        member this.While (guard, m) =
            match guard () with
            | true -> this.Bind (m, fun () -> this.While (guard, m))
            | _ -> this.Zero ()
        
        member this.For (sequence: seq<_>, body) =
            this.Using (sequence.GetEnumerator (), fun enum -> 
                this.While (enum.MoveNext, this.Delay (fun () -> 
                    body enum.Current)))

    /// OWIN Monad
    let owin = new OwinMonadBuilder ()
    
    /// Gets the current OwinEnv within an OWIN monad
    let getM : OwinMonad<OwinEnv> =
        fun env -> async { return env, env }

    /// Sets the OwinEnv within an OWIN monad
    let setM env : OwinMonad<unit> =
        fun _ -> async { return (), env }

    /// Modifies the current OwinEnv within an OWIN monad
    let modM f : OwinMonad<unit> =
        fun env -> async { return (), f env }


[<RequireQualifiedAccess>]
module internal Monadic =

    let inline returnM builder x = 
        (^M: (member Return: 'b -> 'c) (builder, x))

    let inline bindM builder m f = 
        (^M: (member Bind: 'd -> ('e -> 'c) -> 'c) (builder, m, f))

    let inline liftM builder f m =
        let inline ret x = returnM builder (f x)
        bindM builder m ret

    let inline applyM (builder1: ^M1) (builder2: ^M2) f m =
        bindM builder1 f <| fun f' ->
            bindM builder2 m <| fun m' ->
                returnM builder2 (f' m')


module Operators =

    let inline returnM x =
        Monadic.returnM owin x

    let inline (>>=) m1 m2 =
        Monadic.bindM owin m1 m2

    let inline (=<<) m1 m2 =
        Monadic.bindM owin m2 m1

    let inline (<*>) f m =
        Monadic.applyM owin owin f m

    let inline (<!>) f m =
        Monadic.liftM owin f m

    let inline liftAsync f =
        owin { 
            return! (fun env -> 
                async { 
                    let! v = f
                    return v, env }) }
        
    let inline lift2 f m1 m2 =
        returnM f <*> m1 <*> m2 

    let inline ( *>) m1 m2 =
        lift2 (fun _ x -> x) m1 m2

    let inline ( <*) m1 m2 =
        lift2 (fun x _ -> x) m1 m2

    let inline (>>.) m f =
        Monadic.bindM owin m (fun _ -> f)

    let inline (>=>) m1 m2 =
        fun x -> m1 x >>= m2

    let inline (<=<) m1 m2 =
        fun x -> m2 x >>= m1


/// .NET language interop helpers
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module OwinAppFunc =

    /// Converts a F# Async-based OWIN AppFunc to a standard Func<_,Task> AppFunc.
    [<CompiledName("FromOwinApp")>]
    let fromOwinApp (app: OwinApp) =
        OwinAppFunc (fun env -> Async.StartAsTask (app env) :> Task)

    /// Converts a F# OwinMonad<_> to a standard Func<_,Task> AppFunc.
    [<CompiledName("FromOwinMonad")>]
    let fromOwinMonad (monad: OwinMonad<_>) =
        fromOwinApp (fun e -> async { do! monad e |> Async.Ignore })
