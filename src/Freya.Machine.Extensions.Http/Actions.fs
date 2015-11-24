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

[<RequireQualifiedAccess>]
module internal Freya.Machine.Extensions.Http.Actions

open Freya.Core
open Freya.Machine
open Freya.Machine.Operators

(* Actions *)

let private userAction key =
    Some (Compile (Configuration.get key
        >> Option.map (fun x -> Compiled (Unary x, configured))
        >> Option.orElse (Compiled (Unary (Freya.init ()), unconfigured))))

(* Graph *)

let operations =
    [ Operation Actions.Delete                         =.        userAction Actions.Delete
      Operation Actions.Patch                          =.        userAction Actions.Patch
      Operation Actions.Post                           =.        userAction Actions.Post
      Operation Actions.Put                            =.        userAction Actions.Put

      Operation Actions.Delete                         >.        Operation Decisions.Deleted
      Operation Actions.Patch                          >.        Operation Decisions.RespondWithEntity
      Operation Actions.Post                           >.        Operation Decisions.PostRedirect
      Operation Actions.Put                            >.        Operation Decisions.Created ]