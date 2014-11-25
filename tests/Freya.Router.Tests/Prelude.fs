[<AutoOpen>]
module internal Freya.Router.Tests.Prelude

open System.Collections.Generic
open Aether
open Aether.Operators
open Freya.Core
open Freya.Core.Operators
open Freya.Pipeline
open Freya.Router
open Freya.Typed

(* Keys *)

let [<Literal>] testKey =
    "freya.Test"

(* Lenses *)

let private testLens =
    dictPLens testKey <?-> (unbox<int>, box)

(* Functions *)

let get =
    getPL testLens

let set i =
    setPLM testLens i *> next

let private run path m =
    let state = Dictionary<string, obj> ()
    let router = compileFreyaRouter m

    Async.RunSynchronously ((setLM Request.path path *> router) state)

let result path m =
    fst (run path m)

let value path m =
    get (snd (run path m))
