[<AutoOpen>]
module Freya.Inspector.Integration

open Aether
open Aether.Operators
open Freya.Core.Operators
open Freya.Typed

(* Functions *)

let setI<'a> key a =
    modPLM proxyPLens (fun p -> p.Update (setPL (itemPLens<'a> key) a); p)

let modI<'a> key f =
    modPLM proxyPLens (fun p -> p.Update (modPL (itemPLens<'a> key) f); p)
