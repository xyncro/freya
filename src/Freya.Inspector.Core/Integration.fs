[<AutoOpen>]
module Freya.Inspector.Core.Integration

open System
open Aether
open Aether.Operators
open Freya.Core
open Freya.Core.Operators
open Freya.Typed

(* Untyped Entry Integration *)

let initE () =
    let i = Guid.NewGuid ()
    let _ = store.Post (Create i)
    let p = proxy i

    setPLM proxyPLens p

let listE () =
    liftAsync (store.PostAndAsyncReply (fun c -> ReadAll (c)))

(* Typed Inspection Integration *)

let setI<'a> key a =
    modPLM proxyPLens (fun p -> p.Update (setPL (itemPLens<'a> key) a); p)

let modI<'a> key f =
    modPLM proxyPLens (fun p -> p.Update (modPL (itemPLens<'a> key) f); p)