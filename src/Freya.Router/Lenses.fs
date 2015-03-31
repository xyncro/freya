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
module Freya.Router.Lenses

open Aether
open Aether.Operators
open Freya.Core
open Freya.Types.Uri.Template

(* Keys *)

let [<Literal>] private valuesKey =
    "freya.RouterValues"

(* Lenses

   Access to values matched (as UriTemplateData) as part of
   the routing process are accessible via these lenses in to
   the OWIN state. *)

[<RequireQualifiedAccess>]
module Route =

    let data =
             environmentKeyPLens valuesKey 
        <?-> boxIso<UriTemplateData>

    // TODO: Fix this when we come up with a better key representation?

    let item key =
             data 
        <?-> UriTemplateData.UriTemplateDataIso
        >??> mapPLens (Key [ key ])

    let atom key =
             item key
        <??> UriTemplateValue.AtomPIso

    let list key =
             item key
        <??> UriTemplateValue.ListPIso

    let keys key =
             item key
        <??> UriTemplateValue.KeysPIso
