[<AutoOpen>]
module internal Freya.Router.Trie

open Aether
open Aether.Operators
open Freya.Pipeline

(* Types *)

type FreyaRouterTrie =
    { Root: FreyaRouterTrieNode }

and FreyaRouterTrieNode =
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

let private rootLens =
    (fun x -> x.Root), 
    (fun r x -> { x with Root = r })

(* Constructors *)

let private recognizer (key: string) =
    match String.length key with
    | 0 -> Ignore (key)
    | _ -> match key.[0] with
           | ':' -> Capture (key.Substring (1))
           | _ -> Ignore (key)

let private node key =
    { Children = List.empty
      Key = key
      Pipelines = List.empty
      Recognizer = recognizer key }

let private trie =
    { Root = node "" }

let path (p: string) =
    p.Split '/'
    |> List.ofArray
    |> List.filter ((<>) "")

(* Construction *)

let rec private add n =
    function | (h :: t, p, m) -> (find h n |> update h t p m) n
             | (_, p, m) -> modL pipelinesLens (fun ps -> ps @ [ (m, p) ]) n

and private find h =
    function | { Children = x } -> List.tryFindIndex (fun x -> x.Key = h) x

and private update h t p m =
    function | Some i -> extend i t p m
             | _ -> append h t p m

and private extend i t p m =
    modPL (childPLens i) (flip add (t, p, m))

and private append h t p m =
    modL childrenLens (flip (@) [ add (node h) (t, p, m) ])

let private addRoute r =
    modL rootLens ((flip add) (path r.Path, r.Pipeline, r.Method))

let construct =
    List.fold (flip addRoute) trie