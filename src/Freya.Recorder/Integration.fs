[<AutoOpen>]
module Freya.Recorder.Integration

open System
open Aether
open Aether.Operators
open Freya.Core
open Freya.Core.Operators
open Freya.Types

(* Untyped Entry Integration *)

let initR () =
    let i = Guid.NewGuid ()
    let _ = store.Post (Create i)
    let p = proxy i

    setPLM proxyPLens p

let listR () =
    liftAsync (store.PostAndAsyncReply List)

(* Typed Inspection Integration *)

let setR<'a> key a =
    modPLM proxyPLens (fun p -> p.Update (setPL (recordPLens<'a> key) a); p)

let modR<'a> key f =
    modPLM proxyPLens (fun p -> p.Update (modPL (recordPLens<'a> key) f); p)