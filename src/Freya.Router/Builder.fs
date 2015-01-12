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
module Freya.Router.Builder

(* Builder *)

type RouterBuilder () =

    member __.Return v : Router = 
        fun r -> v, r

    member __.ReturnFrom f : Router = 
        f

    member __.Bind (r, k) : Router = 
        r >> fun (result, trie) -> (k result) trie

    member x.Combine (r1, r2) : Router = 
        x.Bind (r1, fun () -> r2)

    member internal x.Update (r, update) = 
        x.Bind ((fun res -> (), update res), fun _ -> x.ReturnFrom r)

let router = RouterBuilder ()