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
    Freya.Optic.set test_ i *> Freya.Pipeline.next

let private run meth path query m =
    let router = FreyaRouter.FreyaPipeline m
    let state = freyaState ()

    Async.RunSynchronously ((   Freya.Optic.set Request.method_ meth
                             *> Freya.Optic.set Request.path_ path
                             *> Freya.Optic.set Request.query_ query
                             *> router) state)

let result meth path query m =
    fst (run meth path query m)

let value meth path query m =
    get (snd (run meth path query m))

(* Routes *)

let route1 =
    set (Some 1)

let route2 =
    set (Some 2)

let route3 =
    set (Some 3)