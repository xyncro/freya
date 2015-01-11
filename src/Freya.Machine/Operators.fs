module Freya.Machine.Operators

(* Definition

   Infix operators for working with definition graphs, producing
   definition graph operations. Combined, these operators enable a
   moderately visual DSL for working with definition graphs. *)

let (.+>) source dest =
    addNewDefinitionEdge source dest (Value (Some true))

let (.->) source dest =
    addNewDefinitionEdge source dest (Value (Some false))

let (..>) source dest =
    addNewDefinitionEdge source dest (Value (None))

let (/*>) =
    removeExistingDefinitionEdge 

let (.|=) id n =
    addNewDefinitionNode id n

let (/|=) id () =
    removeExistingDefinitionNode id