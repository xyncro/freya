[<AutoOpen>]
module internal Freya.Router.Tests.Prelude

open System.Collections.Generic
open Aether
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

(* Functions *)

let private get s =
    Optic.get (Environment.value_ testKey) s

let private set i =
    Freya.Optic.set (Environment.value_ testKey) i

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

let routeF f =
    (f >>= set) *> Freya.Pipeline.next

let private save v =
    routeF (Freya.init v)

let route1 =
    save (Some 1)

let route2 =
    save (Some 2)

let route3 =
    save (Some 3)