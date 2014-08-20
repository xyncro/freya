namespace Dyfrig.Router

open Aether
open Aether.Operators
open Dyfrig

type Router = 
    Routes -> unit * Routes

and Routes =
    Route list

and Route =
    { Methods: Method list option 
      Path: string
      App: OwinMonad<bool> }

type RouterBuilder () =

    member x.Return v : Router = 
        fun r -> v, r

    member x.ReturnFrom f : Router = 
        f

    member x.Bind (r, k) : Router = 
        r >> fun (result, trie) -> (k result) trie

    member x.Combine (r1, r2) : Router = 
        x.Bind (r1, fun () -> r2)

    member internal x.Update (r, update) = 
        x.Bind ((fun res -> (), update res), fun _ -> x.ReturnFrom r)

[<AutoOpen>]
module Expression =

    let router = RouterBuilder ()

[<RequireQualifiedAccess>]
module Routing =
        
    let internal Values =
        owinEnvLens "dyfrig.routingData"
        >--> isoBoxLens<Map<string, string>> 

    let Value key = 
        Values
        >-?> mapPLens key