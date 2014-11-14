[<AutoOpen>]
module Freya.Router.Monad

type FreyaRouter = 
    FreyaRoute list -> unit * FreyaRoute list

type FreyaRouterBuilder () =

    member __.Return v : FreyaRouter = 
        fun r -> v, r

    member __.ReturnFrom f : FreyaRouter = 
        f

    member __.Bind (r, k) : FreyaRouter = 
        r >> fun (result, trie) -> (k result) trie

    member x.Combine (r1, r2) : FreyaRouter = 
        x.Bind (r1, fun () -> r2)

    member internal x.Update (r, update) = 
        x.Bind ((fun res -> (), update res), fun _ -> x.ReturnFrom r)

let freyaRouter = FreyaRouterBuilder ()
