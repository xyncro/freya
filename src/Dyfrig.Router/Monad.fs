[<AutoOpen>]
module Dyfrig.Router.Monad

type RoutesMonad = 
    Routes -> unit * Routes

type RoutesMonadBuilder () =

    member __.Return v : RoutesMonad = 
        fun r -> v, r

    member __.ReturnFrom f : RoutesMonad = 
        f

    member __.Bind (r, k) : RoutesMonad = 
        r >> fun (result, trie) -> (k result) trie

    member x.Combine (r1, r2) : RoutesMonad = 
        x.Bind (r1, fun () -> r2)

    member internal x.Update (r, update) = 
        x.Bind ((fun res -> (), update res), fun _ -> x.ReturnFrom r)

let routes = RoutesMonadBuilder ()