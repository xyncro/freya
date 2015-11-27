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
//
//----------------------------------------------------------------------------

[<AutoOpen>]
module Freya.Router.Builder

(* Type *)

type FreyaRouter = 
    | FreyaRouter of (FreyaRoute list -> unit * FreyaRoute list)

    static member Pipeline (FreyaRouter x) =
        Reification.reify x

(* Builder

   The Computation Expression builder to give Router the declarative
   computation expression syntax for specifying Routes.
   Specific strongly typed custom operations are defined in Syntax.fs. *)

type FreyaRouterBuilder () =

    member __.Return _ =
        FreyaRouter (fun routes -> (), routes)

    member __.ReturnFrom m = 
        m

    member __.Bind (m, k) =
        FreyaRouter (fun routes ->
            let (FreyaRouter m') = m
            let (FreyaRouter k') = k ()

            (), snd (k' (snd (m' routes))))

    member x.Combine (m1, m2) =
        x.Bind (m1, fun () -> m2)

    member x.Map (m, f) = 
        x.Bind (FreyaRouter (fun routes -> (), f routes), fun _ -> m)

let freyaRouter =
    FreyaRouterBuilder ()