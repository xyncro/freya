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
module internal Freya.Machine.Extensions.Http.Prelude

open Freya.Core
open Freya.Machine

(* Configuration Metadata *)

let configured =
    { Configurable = true
      Configured = true }

let unconfigured =
    { Configurable = true
      Configured = false }

let unconfigurable =
    { Configurable = false
      Configured = false }

(* Functions *)

let inline flip f a b = 
    f b a

(* List Extensions *)

[<RequireQualifiedAccess>]
module List =

    let chooseMaxBy projection =
            List.map (fun x -> x, projection x)
         >> List.choose (function | (x, Some y) -> Some (x, y) | _ -> None)
         >> List.sortBy (fun (_, y) -> y)
         >> List.map fst
         >> function | [] -> None | x :: _ -> Some x

(* Option Extensions *)

[<RequireQualifiedAccess>]
module Option =
    
    let orElse def =
        function | Some x -> x
                 | _ -> def