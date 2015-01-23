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

let userAction key =
    Unary (tryGetConfig key
        >> Option.map (fun x -> configured, x)
        >> Option.orElse (unconfigured, Freya.init ()))

(* Graph *)

let operations =
    [ Ref Actions.Delete                         =.        userAction Actions.Delete
      Ref Actions.Patch                          =.        userAction Actions.Patch
      Ref Actions.Post                           =.        userAction Actions.Post
      Ref Actions.Put                            =.        userAction Actions.Put

      Ref Actions.Delete                         >.        Ref Decisions.Deleted
      Ref Actions.Patch                          >.        Ref Decisions.RespondWithEntity
      Ref Actions.Post                           >.        Ref Decisions.PostRedirect
      Ref Actions.Put                            >.        Ref Decisions.Created ]