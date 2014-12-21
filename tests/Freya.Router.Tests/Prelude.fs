[<AutoOpen>]
module internal Freya.Router.Tests.Prelude

open System.Collections.Generic
open Aether
open Aether.Operators
open Freya.Core
open Freya.Core.Operators
open Freya.Pipeline
open Freya.Router
open Freya.Types.Http

(* Keys *)

let [<Literal>] testKey =
    "freya.Test"

(* Lenses *)

let private testLens =
    environmentKeyPLens testKey <?-> (unbox<int>, box)

(* Functions *)

let get =
    getPL testLens

let set i =
    setPLM testLens i *> next

let private run meth path m =
    let state = Dictionary<string, obj> ()
    let router = compileFreyaRouter m

    Async.RunSynchronously ((   setLM Request.path path 
                             *> setLM Request.meth meth 
                             *> router) state)

let result meth path m =
    fst (run meth path m)

let value meth path m =
    get (snd (run meth path m))
