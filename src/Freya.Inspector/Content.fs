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
open Arachne.Uri.Template

(* Content *)

let private cssContent =
    resource "app.css"

let private htmlContent = 
    resource "index.html"

let private jsContent =
    resource "app.js"

(* Functions *)

let private getContent content n =
    represent n <!> Freya.init content

let private getCss =
    getContent cssContent

let private getHtml =
    getContent htmlContent

let private getJs =
    getContent jsContent

(* Resources *)

let private css =
    freyaMachine {
        including defaults
        mediaTypesSupported css
        handleOk getCss } |> FreyaMachine.toPipeline

let private html =
    freyaMachine {
        including defaults
        mediaTypesSupported html
        handleOk getHtml } |> FreyaMachine.toPipeline

let private js =
    freyaMachine {
        including defaults
        mediaTypesSupported js
        handleOk getJs } |> FreyaMachine.toPipeline

(* Routes

   Note: This routing will probably need to be modified to allow for
   additional tools under the /freya/* path namespace at some point, but
   this will require some tweaks to the directory structure of the
   freya.ui.* projects involved. *)

let private root =
    UriTemplate.Parse "/freya/inspector"

let content =
    freyaRouter {
        resource (root) html
        resource (root + UriTemplate.Parse "/css/app.css") css
        resource (root + UriTemplate.Parse "/js/app.js") js } |> FreyaRouter.toPipeline
