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
module Freya.Todo.Backend.Prelude

open System.IO
open System.Text
open Fleece
open Freya.Core
open Freya.Core.Operators
open Freya.Machine
open Freya.Types.Cors
open Freya.Types.Http
open Freya.Types.Language

// Utilities 

let tuple x y =
    x, y

// Presets

let corsOrigins : Freya<AccessControlAllowOriginRange> =
    returnM AccessControlAllowOriginRange.Any

let corsHeaders : Freya<string list> =
    returnM [ "accept"; "content-type" ]

let en : Freya<LanguageTag list> =
    returnM [ LanguageTag.Parse "en" ]

let json : Freya<MediaType list> =
    returnM [ MediaType.JSON ]

let utf8 : Freya<Charset list> =
    returnM [ Charset.UTF8 ]

// Defaults

let defaults =
    freyaMachine {
        charsetsSupported utf8
        corsHeadersSupported corsHeaders
        corsOriginsSupported corsOrigins
        languagesSupported en
        mediaTypesSupported json }

// Body

let readStream (x: Stream) =
    use reader = new StreamReader (x)
    reader.ReadToEnd ()

let readBody () =
    readStream <!> getLM Request.body

let inline body () =
    (function | Choice1Of2 x -> Some x | _ -> None) <!> (parseJSON <!> readBody ())

// Representation

let inline represent x =
    { Metadata =
        { Charset = Some Charset.UTF8
          Encodings = None
          MediaType = Some MediaType.JSON
          Languages = Some [ LanguageTag.Parse "en" ] }
      Data = (toJSON >> string >> Encoding.UTF8.GetBytes) x }