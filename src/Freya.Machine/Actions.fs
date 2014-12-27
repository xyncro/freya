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
module internal Freya.Machine.Actions

open Freya.Core.Operators

(* Actions

   Action nodes execute some kind of "side-effecting" logical action
   (i.e. in response to a DELETE, POST, etc. method which is generally
   non-idempotent). They will generally need overriding if the resource
   is going to support the associated method. *)

let private action =
    returnM ()

let private actionDefinitions =
    [ Actions.Delete,                         Decisions.Deleted 
      Actions.Patch,                          Decisions.RespondWithEntity
      Actions.Post,                           Decisions.PostRedirect
      Actions.Put,                            Decisions.Created ]

let actions =
    actionDefinitions
    |> List.map (fun (id, next) ->
            ActionNode { Id = id
                         Override = 
                           { Allow = true
                             Overridden = false }
                         Action = action
                         Next = next })
