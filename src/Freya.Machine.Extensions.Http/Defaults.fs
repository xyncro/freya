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
module internal Freya.Machine.Extensions.Http.Defaults

open System
open Freya.Core
open Arachne.Http
open Arachne.Language

(* Charsets *)

let charsetsSupported =
    Freya.init [ Charset.Iso88591 ]

(* Encodings *)

let encodingsSupported =
    Freya.init List.empty<ContentCoding>

(* Languages *)

let languagesSupported =
    Freya.init List.empty<LanguageTag>

(* Media Types *)

let mediaTypesSupported =
    Freya.init List.empty<MediaType>

(* Methods *)

let methodsKnown =
    Freya.init [
        CONNECT
        DELETE
        HEAD
        GET
        OPTIONS
        POST
        PUT
        TRACE
        Method.Custom "PATCH" ]

let methodsSupported =
    Freya.init [
        GET
        HEAD ]