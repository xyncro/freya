[<AutoOpen>]
module Dyfrig.Inspector.Inspection

open Aether
open Aether.Operators
open Dyfrig.Core.Operators
open Dyfrig.Http

(* Functions *)

let getI<'a> key =
    Option.bind (fun p -> Option.bind (getPL (itemPLens<'a> key)) (p.Read ())) <!> getPLM protocolPLens

let setI<'a> key a =
    modPLM protocolPLens (fun p -> p.Update (setPL (itemPLens<'a> key) a); p)

let modI<'a> key f =
    modPLM protocolPLens (fun p -> p.Update (modPL (itemPLens<'a> key) f); p)