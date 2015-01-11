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

let inline (>>=) m f =
    Freya.bind m f

let inline (=<<) f m =
    Freya.bind m f

let inline (<*>) f m =
    Freya.apply f m

let inline (<!>) f m =
    Freya.map f m

let inline ( *>) m1 m2 =
    Freya.map2 (fun _ x -> x) m1 m2

let inline ( <*) m1 m2 =
    Freya.map2 (fun x _ -> x) m1 m2

let inline (>>.) m f =
    Freya.bind m (fun _ -> f)

let inline (>=>) m1 m2 =
    fun x -> m1 x >>= m2

let inline (<=<) m1 m2 =
    fun x -> m2 x >>= m1
