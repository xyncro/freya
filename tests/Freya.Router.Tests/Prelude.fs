[<AutoOpen>]
module internal Freya.Router.Tests.Prelude

open System.Collections.Generic
open Aether
open Aether.Operators
open Arachne.Http
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

let private testLens =
    Environment.optional testKey <?-> (unbox<int>, box)

(* Functions *)

let private get =
    Lens.getPartial testLens

let private set i =
    Freya.setLensPartial testLens i *> Freya.next

let private run meth path m =
    let router = FreyaRouter.toPipeline m

    Async.RunSynchronously ((   Freya.setLens Request.path path 
                             *> Freya.setLens Request.meth meth 
                             *> router) (freyaState ()))

let result meth path m =
    fst (run meth path m)

let value meth path m =
    get (snd (run meth path m))

(* Methods *)

let Get =
    Methods [ GET ]

let Post =
    Methods [ POST ]

(* Routes *)

let route1 =
    set 1

let route2 =
    set 2

let route3 =
    set 3