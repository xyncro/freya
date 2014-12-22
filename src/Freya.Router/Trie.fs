[<AutoOpen>]
module internal Freya.Router.Trie

open Aether
open Aether.Operators
open Freya.Pipeline

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

(* Lenses *)

let private childPLens i =
    FreyaRouterTrie.ChildrenLens >-?> listPLens i

(* Build *)

let rec private add node =
    function | (segment :: path, pipeline, meth) -> 
                (find segment node |> update segment path pipeline meth) node
             | (_, pipeline, meth) -> 
                modL FreyaRouterTrie.PipelinesLens (fun ps -> ps @ [ (meth, pipeline) ]) node

and private find segment =
    function | { Children = x } -> List.tryFindIndex (fun x -> x.Key = segment) x

and private update segment path pipeline meth =
    function | Some i -> extend i path pipeline meth
             | _ -> append segment path pipeline meth

and private extend i path pipeline meth =
    modPL (childPLens i) (flip add (path, pipeline, meth))

and private append segment path pipeline meth =
    modL FreyaRouterTrie.ChildrenLens (flip (@) [ add (node segment) (path, pipeline, meth) ])

let private addRoute route =
    (flip add) (segmentize route.Path, route.Pipeline, route.Method)

let buildTrie =
    List.fold (flip addRoute) (node "")