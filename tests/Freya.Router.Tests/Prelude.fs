[<AutoOpen>]
module internal Freya.Router.Tests.Prelude

open System.Collections.Generic
open Aether
open Aether.Operators
open Arachne.Http
open Arachne.Uri
open Freya.Core
open Freya.Core.Operators
open Freya.Lenses.Http
open Freya.Router

let private freyaState () =
    let env = 
        Dictionary<string, obj> () :> IDictionary<string, obj>

    { Environment = env
      Meta =
        { Memos = Map.empty } }

(* Keys *)

let [<Literal>] testKey =
    "freya.Test"

(* Lenses *)

let private test_ =
    Environment.value_ testKey

(* Functions *)

let private get =
    Lens.get test_

let private set i =
    Freya.Lens.set test_ i *> Freya.next

let private run meth path query m =
    let router = FreyaRouter.toPipeline m
    let state = freyaState ()

    Async.RunSynchronously ((   Freya.Lens.set Request.method_ meth
                             *> Freya.Lens.set Request.path_ path
                             *> Freya.Lens.set Request.query_ query
                             *> router) state)

let result meth path query m =
    fst (run meth path query m)

let value meth path query m =
    get (snd (run meth path query m))

(* Methods *)

let Get =
    Methods [ GET ]

let Post =
    Methods [ POST ]

(* Routes *)

let route1 =
    set (Some 1)

let route2 =
    set (Some 2)

let route3 =
    set (Some 3)