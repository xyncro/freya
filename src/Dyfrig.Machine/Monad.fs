[<AutoOpen>]
module Dyfrig.Machine.Monad

open Aether

(* Monad
        
   The monad to give Machine the declarative computation
   expression syntax for specifying Machine Definitions. Specific strongly 
   typed custom operations are defined in Machine.Syntax.fs. *)

type MachineMonad = 
    MachineDefinition -> unit * MachineDefinition

type MachineMonadBuilder () =

    member __.Return _ : MachineMonad =
        fun definition -> (), definition

    member __.ReturnFrom machine : MachineMonad = 
        machine

    member __.Bind (m, k) : MachineMonad = 
        m >> fun (result, definition) -> (k result) definition

    member x.Combine (m1, m2) : MachineMonad = 
        x.Bind (m1, fun () -> m2)

    member internal x.Set (r, lens, value) = 
        x.Bind ((fun res -> (), setPL lens value res), fun _ -> x.ReturnFrom r)

let machine = MachineMonadBuilder ()