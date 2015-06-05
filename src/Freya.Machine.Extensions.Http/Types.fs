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
module Freya.Machine.Extensions.Http.Types

open Arachne.Http
open Arachne.Language

(* Negotiation *)

type Negotiation<'a> =
    | Negotiated of 'a list
    | Free

(* Representation *)

type Representation =
    { Data: byte []
      Description: Description }

and Specification =
    { Charsets: Negotiation<Charset>
      Encodings: Negotiation<ContentCoding>
      MediaTypes: Negotiation<MediaType>
      Languages: Negotiation<LanguageTag> }

and Description =
    { Charset: Charset option
      Encodings: ContentCoding list option
      MediaType: MediaType option
      Languages: LanguageTag list option }