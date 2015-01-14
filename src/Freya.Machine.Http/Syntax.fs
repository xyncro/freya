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
module Freya.Machine.Http.Syntax

open Aether
open Freya.Core
open Freya.Machine

(* Helpers *)

let private set<'a> key value =
    modL FreyaMachineSpecification.ConfigurationLens (setConfiguration<'a> key value)

(* Common *)

type FreyaMachineBuilder with

    (* Handlers *)

    [<CustomOperation (Common.Handlers.ExpectationFailed, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleExpectationFailed (m, expectationFailed: Freya<unit>) =
        x.Map (m, set Common.Handlers.ExpectationFailed expectationFailed)

    [<CustomOperation (Common.Handlers.HeadersTooLarge, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleHeadersTooLarge (m, headersTooLarge: Freya<unit>) =
        x.Map (m, set Common.Handlers.HeadersTooLarge headersTooLarge)

    [<CustomOperation (Common.Handlers.NotImplemented, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleNotImplemented (m, notImplemented: Freya<unit>) =
        x.Map (m, set Common.Handlers.NotImplemented notImplemented)

    [<CustomOperation (Common.Handlers.InternalServerError, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleInternalServerError (m, internalServerError: Freya<unit>) =
        x.Map (m, set Common.Handlers.InternalServerError internalServerError)

    [<CustomOperation (Common.Handlers.ServiceUnavailable, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleServiceUnavailable (m, serviceUnavailable: Freya<unit>) =
        x.Map (m, set Common.Handlers.ServiceUnavailable serviceUnavailable)

    [<CustomOperation (Common.Handlers.UriTooLong, MaintainsVariableSpaceUsingBind = true)>]
    member x.HandleUriTooLong (m, uriTooLong: Freya<unit>) =
        x.Map (m, set Common.Handlers.UriTooLong uriTooLong)

(* System

   Custom operators supporting the decision making process defined
   as part of the System section of the HTTP graph. *)

type FreyaMachineBuilder with

    (* Decisions *)

    [<CustomOperation (System.Decisions.AreHeadersTooLarge, MaintainsVariableSpaceUsingBind = true)>]
    member x.AreHeadersTooLarge (m, areHeadersTooLarge: Freya<bool>) =
        x.Map (m, set System.Decisions.AreHeadersTooLarge areHeadersTooLarge)

    [<CustomOperation (System.Decisions.IsFunctionalityImplemented, MaintainsVariableSpaceUsingBind = true)>]
    member x.IsFunctionalityImplemented (m, isFunctionalityImplemented: Freya<bool>) =
        x.Map (m, set System.Decisions.IsFunctionalityImplemented isFunctionalityImplemented)

    [<CustomOperation (System.Decisions.IsServiceAvailable, MaintainsVariableSpaceUsingBind = true)>]
    member x.IsServiceAvailable (m, isServiceAvailable: Freya<bool>) =
        x.Map (m, set System.Decisions.IsServiceAvailable isServiceAvailable)

    [<CustomOperation (System.Decisions.IsSystemOk, MaintainsVariableSpaceUsingBind = true)>]
    member x.IsSystemOk (m, isSystemOk: Freya<bool>) =
        x.Map (m, set System.Decisions.IsSystemOk isSystemOk)

    [<CustomOperation (System.Decisions.IsUriTooLong, MaintainsVariableSpaceUsingBind = true)>]
    member x.IsUriTooLong (m, uriTooLong: Freya<bool>) =
        x.Map (m, set System.Decisions.IsUriTooLong uriTooLong)