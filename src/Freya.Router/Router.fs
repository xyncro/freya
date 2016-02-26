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
module Freya.Router.Router

open System
open Freya.Core

(* Type *)

type FreyaRouter = 
    | FreyaRouter of (FreyaRoute list -> unit * FreyaRoute list)

    static member Freya (FreyaRouter x) : Freya<_> =
        Reification.reify x

    static member FreyaPipeline (FreyaRouter x) : FreyaPipeline =
        Reification.reify x

(* Obsolete

   Backwards compatibility shims to make the 2.x-> 3.x transition
   less painful, providing functionally equivalent options where possible.

   To be removed for 4.x releases. *)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module FreyaRouter =

    [<Obsolete ("Explicit conversion to FreyaPipeline is no longer required in Freya.")>]
    let toPipeline =
        id