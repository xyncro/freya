[<AutoOpen>]
module internal Freya.Router.Tests.Prelude

open System.Collections.Generic
open Aether.Operators
open Freya.Core
open Freya.Core.Operators
open Freya.Router
open Freya.Typed

(* Keys *)

let [<Literal>] private testKey =
    "freya.Test"

(* Lenses *)

let private testLens =
    dictLens testKey <--> (unbox<int>, box)

(* Functions *)

let get : Freya<int> =
    getLM testLens

let set i =
    setLM testLens i

let private run path m =
    let state = Dictionary<string, obj> ()
    let router = compileFreyaRouter m

    Async.RunSynchronously ((setLM Request.path path *> router) state)

let result path m =
    fst (run path m)

let state path m =
    snd (run path m)
