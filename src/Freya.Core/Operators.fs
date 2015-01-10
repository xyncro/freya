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

module Freya.Core.Operators

(* Operators *)

/// Wraps a value x in a <see cref="Freya{T}" /> computation.
let inline returnM x : Freya<'T> = 
    fun env -> 
        async.Return (x, env)

/// Applies a function of a value to an <see cref="Async{T}" /> result
/// into a <see cref="Freya{T}" /> computation.
let inline asyncM f =
    (fun f -> 
        fun env -> 
            async { 
                let! v = f
                return v, env }) << f

/// Binds a <see cref="Freya{T}" /> computation with a function that
/// takes the value from the <see cref="Freya{T}" /> computation and
/// computes a new <see cref="Freya{T}" /> computation of a possibly
/// different type.
let inline bindM m f : Freya<'T> =
    fun s -> 
        async { 
            let! r, s = m s
            return! (f r) s }

let inline (>>=) m f =
    bindM m f

let inline (=<<) f m =
    bindM m f

let inline applyM f m : Freya<'T> =
    f >>= fun f' ->
    m >>= fun m' ->
    returnM (f' m')

let inline (<*>) f m =
    applyM f m

let inline liftM f m : Freya<'T> =
    returnM f <*> m

let inline (<!>) f m =
    liftM f m

let inline lift2 f m1 m2 =
    returnM f <*> m1 <*> m2 

let inline ( *>) m1 m2 =
    lift2 (fun _ x -> x) m1 m2

let inline ( <*) m1 m2 =
    lift2 (fun x _ -> x) m1 m2

let inline (>>.) m f =
    bindM m (fun _ -> f)

let inline (>=>) m1 m2 =
    fun x -> m1 x >>= m2

let inline (<=<) m1 m2 =
    fun x -> m2 x >>= m1