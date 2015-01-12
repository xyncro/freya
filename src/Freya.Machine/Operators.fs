module Freya.Machine.Operators

(* Definition

   Infix operators for working with definition graphs, producing
   definition graph operations. Combined, these operators enable a
   moderately visual DSL for working with definition graphs. *)

let (.+>) source dest =
    addNewEdge source dest (Value (Some true))

let (.->) source dest =
    addNewEdge source dest (Value (Some false))

let (..>) source dest =
    addNewEdge source dest (Value (None))

let (/*>) =
    removeExistingEdge 

let (.|=) id n =
    addNewNode id n

let (/|=) id () =
    removeExistingNode id