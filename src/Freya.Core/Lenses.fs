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
module Freya.Core.Lenses

open Aether
open Aether.Operators

[<RequireQualifiedAccess>]
module Environment =

    let value_<'a> k =
            FreyaState.Environment_
       >--> Dict.value_<string, obj> k
       <--> Option.mapIsomorphism box_<'a>

[<RequireQualifiedAccess>]
module Memo =

    let id_<'a> i =
            FreyaState.Meta_
       >--> FreyaMetaState.Memos_
       >--> Map.value_ i
       <--> Option.mapIsomorphism box_<'a>