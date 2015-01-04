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
module internal Freya.Inspector.Content

open Freya.Core.Operators
open Freya.Machine
open Freya.Router
open Freya.Types.Http

(* Content *)

let private cssContent =
    resource "site.css"

let private htmlContent = 
    resource "index.html"

(* Functions *)

let private getContent content n =
    represent n <!> returnM content

let private getCss =
    getContent cssContent

let private getHtml =
    getContent htmlContent

(* Resources *)

let private css =
    freyaMachine {
        including defaults
        mediaTypesSupported css
        handleOk getCss } |> compileFreyaMachine

let private html =
    freyaMachine {
        including defaults
        mediaTypesSupported html
        handleOk getHtml } |> compileFreyaMachine

(* Routes *)

let content =
    freyaRouter {
        route All "/freya" html
        route All "/freya/css" css } |> compileFreyaRouter