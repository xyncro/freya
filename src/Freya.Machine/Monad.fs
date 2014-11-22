[<AutoOpen>]
module Freya.Machine.Monad

open Aether

(* Monad

   The monad to give Machine the declarative computation
   expression syntax for specifying Machine Definitions. Specific strongly 
   typed custom operations are defined in Machine.Syntax.fs. *)

type FreyaMachine = 
    FreyaMachineDefinition -> unit * FreyaMachineDefinition

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
