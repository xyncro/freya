[<AutoOpen>]
module Freya.Router.Execution

open Freya.Core
open Freya.Core.Operators
open Freya.Pipeline
open Freya.Typed

(* Traversal *)

// TODO: Convert these functions to Freya monads

let rec private tryFindNode n m =
    function | h :: t -> n.Children |> pick m h |> ret t
             | _ -> Some (n.Pipelines, m)

and private pick m h =
    function | [] -> None
             | xs -> List.tryPick (fun x -> recognize m h x x.Recognizer) xs

and private recognize m v n =
    function | Capture x -> Some (n, Map.add x v m)
             | Ignore x when x = v -> Some (n, m)
             | _ -> None

and private ret t =
    function | Some (n, m) -> tryFindNode n m t
             | _ -> None

(* Match *)

let private tryFindPipeline meth =
    function | [] -> None
             | xs -> List.tryFind (function | (Methods m, _) -> List.exists ((=) meth) m 
                                            | _ -> true) xs

(* Search *)

let private search trie m p =
    match tryFindNode trie.Root Map.empty (path p) with
    | Some (pipelines, data) -> 
        match tryFindPipeline m pipelines with
        | Some (_, pipeline) -> Some (pipeline, data)
        | _ -> None
    | _ -> None

(* Compilation *)

let compileFreyaRouter (m: FreyaRouter) : FreyaPipeline =
    let _, routes = m List.empty
    let search = search (construct routes)

    freya {
        let! m = getLM Request.meth
        let! p = getLM Request.path

        match search m p with
        | Some (pipeline, data) -> return! setLM Route.Values data *> pipeline
        | _ -> return Next }