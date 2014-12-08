[<AutoOpen>]
module Freya.Router.Execution

open Freya.Core
open Freya.Core.Operators
open Freya.Pipeline
open Freya.Types
open Freya.Types.Http

(* Find *)

let rec private findNode path data node  =
    freya {
        match path with
        | segment :: path -> return! (pick segment data >=> ret path) node.Children
        | _ -> return Some (node.Pipelines, data) }

and private ret path x =
    freya {
        match x with
        | Some (node, data) -> return! findNode path data node
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

let private find meth x =
    freya {
        return List.tryFind (function | (Methods m, _) -> List.exists ((=) meth) m
                                      | _ -> true) x }

let private pair data x =
    freya {
        return Option.map (fun (_, pipeline) -> pipeline, data) x }

let private matchMethod meth x =
    freya {
        match x with
        | Some (pipelines, data) -> return! (find meth >=> pair data) pipelines
        | _ -> return None }

(* Search *)

let private search path meth data trie =
    freya {
        return! (findNode path data >=> matchMethod meth) trie }

(* Compilation *)

let compileFreyaRouter (router: FreyaRouter) : FreyaPipeline =
    let routes = snd (router List.empty)
    let trie = construct routes

    freya {
        do! initR ()

        let! meth = getLM Request.meth
        let! path = segmentize <!> getLM Request.path
        let! x = search path meth Map.empty trie

        match x with
        | Some (pipeline, data) -> return! setLM Route.values data *> pipeline
        | _ -> return Next }