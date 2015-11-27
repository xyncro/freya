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
module internal Freya.Inspector.Content

open Freya.Core
open Freya.Core.Operators
open Freya.Machine
open Freya.Machine.Extensions.Http
open Freya.Machine.Router
open Freya.Router
open Arachne.Http
open Arachne.Uri.Template

(* Functions *)

let private send content n =
        represent n
    <!> Freya.init (resource content)

(* Resources *)

let private css =
    freyaMachine {
        including defaults
        mediaTypesSupported MediaType.Css
        handleOk (send "app.css") }

let private html =
    freyaMachine {
        including defaults
        mediaTypesSupported MediaType.Html
        handleOk (send "index.html") }

let private js =
    freyaMachine {
        including defaults
        mediaTypesSupported MediaType.JavaScript
        handleOk (send "app.js") }

(* Routes

   Note: This routing will probably need to be modified to allow for
   additional tools under the /freya/* path namespace at some point, but
   this will require some tweaks to the directory structure of the
   freya.ui.* projects involved. *)

let private root =
    "/freya/inspector"

let content =
    freyaRouter {
        resource (root) html
        resource (root + "/css/app.css") css
        resource (root + "/js/app.js") js }
