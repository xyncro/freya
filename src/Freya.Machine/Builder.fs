[<AutoOpen>]
module Freya.Machine.Builder

open Aether

(* Builder

   The Computation Expression builder to give Machine the declarative
   computation expression syntax for specifying Machine Definitions.
   Specific strongly typed custom operations are defined in
   Machine.Syntax.fs. *)

type FreyaMachineBuilder () =

    member __.Return _ : FreyaMachine =
        fun definition -> (), definition

    member __.ReturnFrom machine : FreyaMachine = 
        machine

    member __.Bind (m, k) : FreyaMachine = 
        m >> fun (result, definition) -> (k result) definition

    member x.Combine (m1, m2) : FreyaMachine = 
        x.Bind (m1, fun () -> m2)

    member internal x.Set (r, lens, value) = 
        x.Bind ((fun res -> (), setPL lens value res), fun _ -> x.ReturnFrom r)

let freyaMachine = FreyaMachineBuilder ()
