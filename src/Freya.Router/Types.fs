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
module Freya.Router.Types

open Freya.Pipeline
open Freya.Types.Http

(* Routes *)

type FreyaRoute =
    { Method: FreyaRouteMethod
      Path: string
      Pipeline: FreyaPipeline }

and FreyaRouteMethod =
    | All
    | Methods of Method list

type FreyaRouteData =
    Map<string, string>

(* Monad *)

type FreyaRouter = 
    FreyaRoute list -> unit * FreyaRoute list

(* Trie *)

type internal FreyaRouterTrie =
    { Children: FreyaRouterTrie list
      Key: string
      Pipelines: (FreyaRouteMethod * FreyaPipeline) list
      Recognizer: FreyaRouterRecognizer }

    static member ChildrenLens =
        (fun x -> x.Children), 
        (fun c x -> { x with Children = c })

    static member PipelinesLens =
        (fun x -> x.Pipelines), 
        (fun p x -> { x with Pipelines = p })

and internal FreyaRouterRecognizer =
    | Ignore of string
    | Capture of string