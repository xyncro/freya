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
type FreyaBuilder () =

    member __.Return (t) : Freya<'T> =
        Freya.init t
    
    member __.ReturnFrom (m: Freya<'T>) =
        m
    
    member __.Bind (m1: Freya<'T>, m2: 'T -> Freya<'U>) : Freya<'U> =
        Freya.bind m1 m2
    
    member __.Zero () =
        Freya.init ()
    
    member __.Combine (m1: Freya<unit>, m2: Freya<'T>) : Freya<'T> =
        m1 >>= fun () -> m2
    
    member __.TryWith (m: Freya<'T>, handler: exn -> Freya<'T>) : Freya<'T> =
        fun env ->
            try m env
            with e -> (handler e) env
    
    member __.TryFinally (m: Freya<'T>, compensation) : Freya<'T> =
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
let freya = new FreyaBuilder ()
