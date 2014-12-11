module Freya.Core.Operators

(* Operators *)

let inline returnM x =
    returnM freya x

let inline asyncM f =
    (fun f -> 
        fun env -> 
            async { 
                let! v = f
                return v, env }) << f

let inline (>>=) m1 m2 =
    bindM freya m1 m2

let inline (=<<) m1 m2 =
    bindM freya m2 m1

let inline (<*>) f m =
    applyM freya freya f m

let inline (<!>) f m =
    liftM freya f m


        
let inline lift2 f m1 m2 =
    returnM f <*> m1 <*> m2 

let inline ( *>) m1 m2 =
    lift2 (fun _ x -> x) m1 m2

let inline ( <*) m1 m2 =
    lift2 (fun x _ -> x) m1 m2

let inline (>>.) m f =
    bindM freya m (fun _ -> f)

let inline (>=>) m1 m2 =
    fun x -> m1 x >>= m2

let inline (<=<) m1 m2 =
    fun x -> m2 x >>= m1