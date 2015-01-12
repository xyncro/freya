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
module Freya.Core.Builder

open System
open Freya.Core
open Freya.Core.Operators

(* Builder *)

/// Defines a computation expression builder for constructing <see cref="Freya{T}" /> computations.
type CoreBuilder () =

    member __.Return (t) : Core<'T> =
        Core.returnM t
    
    member __.ReturnFrom (m: Core<'T>) =
        m
    
    member __.Bind (m1: Core<'T>, m2: 'T -> Core<'U>) : Core<'U> =
        Core.bindM m1 m2
    
    member __.Zero () =
        Core.returnM ()
    
    member __.Combine (m1: Core<unit>, m2: Core<'T>) : Core<'T> =
        m1 >>= fun () -> m2
    
    member __.TryWith (m: Core<'T>, handler: exn -> Core<'T>) : Core<'T> =
        fun env ->
            try m env
            with e -> (handler e) env
    
    member __.TryFinally (m: Core<'T>, compensation) : Core<'T> =
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

/// The instance of <see cref="FreyaBuilder" /> used for constructing
/// <see cref="Freya{T}" /> computations.
let freyaCore = new CoreBuilder ()
