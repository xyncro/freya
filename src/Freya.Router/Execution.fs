[<AutoOpen>]
module Freya.Router.Execution

open Freya.Core
open Freya.Core.Operators
open Freya.Pipeline
open Freya.Typed

(* Traversal *)

// TODO: Convert these functions to Freya monads

let rec private traverse n m =
    function | h :: t -> n.Children |> pick m h |> ret t
             | _ -> Option.map (fun x -> x, m) n.Pipeline

and private pick m h =
    function | [] -> None
             | xs -> List.tryPick (fun x -> recognize m h x x.Recognizer) xs

and private recognize m v n =
    function | Capture x -> Some (n, Map.add x v m)
             | Ignore x when x = v -> Some (n, m)
             | _ -> None

and private ret t =
    function | Some (n, m) -> traverse n m t
             | _ -> None

let private lookup p trie =
    traverse trie.Root Map.empty (path p)

(* Compilation *)

let compileFreyaRouter (m: FreyaRouter) : FreyaPipeline =
    let _, routes = m List.empty
    let trie = construct routes

    freya {
        let! path = getLM Request.path

        match lookup path trie with
        | Some (app, data) -> return! setLM Route.Values data *> app
        | _ -> return Next }