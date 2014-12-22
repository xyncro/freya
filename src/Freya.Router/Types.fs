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