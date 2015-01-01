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

let inline returnM x =
    returnM freya x

let inline asyncM f =
    (fun f -> 
        fun env -> 
            async { 
                let! v = f
                return v, env }) << f

let inline (>>=) m1 m2 =
    bindM freya m1 m2

let inline (=<<) m1 m2 =
    bindM freya m2 m1

let inline (<*>) f m =
    applyM freya freya f m

let inline (<!>) f m =
    liftM freya f m

let inline lift2 f m1 m2 =
    returnM f <*> m1 <*> m2 

let inline ( *>) m1 m2 =
    lift2 (fun _ x -> x) m1 m2

let inline ( <*) m1 m2 =
    lift2 (fun x _ -> x) m1 m2

let inline (>>.) m f =
    bindM freya m (fun _ -> f)

let inline (>=>) m1 m2 =
    fun x -> m1 x >>= m2

let inline (<=<) m1 m2 =
    fun x -> m2 x >>= m1