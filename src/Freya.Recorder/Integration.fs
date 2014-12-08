[<AutoOpen>]
module Freya.Recorder.Integration

open Freya.Core
open Freya.Core.Operators

(* Functions *)

let initR () =
    setPLM requestIdPLens =<< liftAsync (store.PostAndAsyncReply (fun c -> Create (c))) 

let listR () =
    liftAsync (store.PostAndAsyncReply (fun c -> List (c)))

let getR id =
    liftAsync (store.PostAndAsyncReply (fun c -> Read (id, c)))

let modR f =
    Option.iter (fun id -> store.Post (Update (id, f))) <!> getPLM requestIdPLens