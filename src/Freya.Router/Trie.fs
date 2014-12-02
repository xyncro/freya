[<AutoOpen>]
module internal Freya.Router.Trie

open Aether
open Aether.Operators
open Freya.Pipeline

(* Types *)

type FreyaRouterTrieNode =
    { Children: FreyaRouterTrieNode list
      Key: string
      Pipelines: (FreyaRouteMethod * FreyaPipeline) list
      Recognizer: FreyaRouterRecognizer }

and FreyaRouterRecognizer =
    | Ignore of string
    | Capture of string

(* Lenses *)

let private childrenLens =
    (fun x -> x.Children), 
    (fun c x -> { x with Children = c })

let private childPLens i =
    childrenLens >-?> listPLens i

let private pipelinesLens =
    (fun x -> x.Pipelines), 
    (fun p x -> { x with FreyaRouterTrieNode.Pipelines = p })

(* Constructors *)

let private recognizer (key: string) =
    match key.StartsWith ":" with
    | true -> Capture (key.Substring (1))
    | _ -> Ignore (key)

let private node key =
    { Children = List.empty
      Key = key
      Pipelines = List.empty
      Recognizer = recognizer key }

let segmentize (path: string) =
    path.Split '/'
    |> List.ofArray
    |> List.filter ((<>) "")

(* Construction *)

let rec private add node =
    function | (segment :: path, pipeline, meth) -> (find segment node |> update segment path pipeline meth) node
             | (_, pipeline, meth) -> modL pipelinesLens (fun ps -> ps @ [ (meth, pipeline) ]) node

and private find segment =
    function | { Children = x } -> List.tryFindIndex (fun x -> x.Key = segment) x

and private update segment path pipeline meth =
    function | Some i -> extend i path pipeline meth
             | _ -> append segment path pipeline meth

and private extend i path pipeline meth =
    modPL (childPLens i) (flip add (path, pipeline, meth))

and private append segment path pipeline meth =
    modL childrenLens (flip (@) [ add (node segment) (path, pipeline, meth) ])

let private addRoute route =
    (flip add) (segmentize route.Path, route.Pipeline, route.Method)

let construct =
    List.fold (flip addRoute) (node "")