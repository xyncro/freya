[<AutoOpen>]
module Freya.Router.Execution

open Freya.Core
open Freya.Core.Operators
open Freya.Pipeline
open Freya.Typed

(* Traversal *)

let rec private tryFindNode path data node  =
    freya {
        match path with
        | segment :: path -> return! (pick segment data >=> ret path) node.Children
        | _ -> return Some (node.Pipelines, data) }

and private ret path x =
    freya {
        match x with
        | Some (node, data) -> return! tryFindNode path data node
        | _ -> return None }

and private pick segment data nodes =
    freya {
        match nodes with
        | [] -> return None
        | nodes ->
            let! env = getM

            let x, env =
                List.fold (fun (x, env) node -> 
                    match x with
                    | Some (node, data) -> (Some (node, data), env)
                    | None -> Async.RunSynchronously (recognize segment data node env)) (None, env) nodes

            do! setM env

            return x }

and private recognize segment data node =
    freya {
        match node.Recognizer with
        | Capture x -> return Some (node, Map.add x segment data)
        | Ignore x when x = segment -> return Some (node, data)
        | _ -> return None }

(* Match *)

let private tryFindPipeline meth xs =
    freya {
        match xs with
        | [] -> return None
        | xs -> return List.tryFind (function | (Methods m, _) -> List.exists ((=) meth) m 
                                              | _ -> true) xs }

(* Search *)

// TODO: Factor out nested matches.
// Probably change tryFindPipeline so composable with tryFindNode (>=>)

let private search path meth trie =
    freya {
        let! x = tryFindNode path Map.empty trie.Root

        match x with
        | Some (pipelines, data) ->
            let! pipeline = tryFindPipeline meth pipelines

            match pipeline with
            | Some (_, pipeline) -> return Some (pipeline, data)
            | _ -> return None
        | _ -> return None }

(* Compilation *)

let compileFreyaRouter (m: FreyaRouter) : FreyaPipeline =
    let _, routes = m List.empty
    let trie = construct routes

    freya {
        let! m = getLM Request.meth
        let! p = path <!> getLM Request.path
        let! x = search p m trie

        match x with
        | Some (pipeline, data) -> return! setLM Route.Values data *> pipeline
        | _ -> return Next }