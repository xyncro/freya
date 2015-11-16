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

/// Custom operators for composing <see cref="Freya{T}" /> computations.
module Freya.Core.Operators

(* Compositional

   Operators for the composition of Freya<'T> functions in
   various forms. *)

let inline (>>=) m f =
    Freya.bind f m

let inline (=<<) f m =
    Freya.bind f m

let inline (<*>) f m =
    Freya.apply f m

let inline (<!>) f m =
    Freya.map f m

let inline ( *>) m1 m2 =
    Freya.map2 (fun _ x -> x) m1 m2

let inline ( <*) m1 m2 =
    Freya.map2 (fun x _ -> x) m1 m2

let inline (>>.) m f =
    Freya.bind (fun _ -> f) m

let inline (>=>) m1 m2 =
    fun x -> m1 x >>= m2

let inline (<=<) m1 m2 =
    fun x -> m2 x >>= m1

(* Lens

   Operators for lens based operations over the Freya<'T> state,
   providing operator based alternatives to Freya.getLens, etc. *)

let inline (!.) l =
    Freya.Lens.get l

let inline (.=) l v =
    Freya.Lens.set l v

let inline (%=) l f =
    Freya.Lens.map l f

(* Prism *)

let inline (!?.) p =
    Freya.Prism.get p

let inline (.?=) p v =
    Freya.Prism.set p v

let inline (%?=) p f =
    Freya.Prism.map p f

(* Pipeline *)

let inline (>?=) p1 p2 = 
    Freya.pipe p1 p2