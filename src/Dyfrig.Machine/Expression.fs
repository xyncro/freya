namespace Dyfrig.Machine

open Aether
open FSharpx

module A = Actions
module C = Config
module D = Decisions
module H = Handlers

type MachineMonad = 
    Definition -> unit * Definition

type MachineMonadBuilder () =

    member x.Return (t) : MachineMonad = 
        tuple2 t

    member x.ReturnFrom f : MachineMonad = 
        f

    member x.Bind (m, k) : MachineMonad = 
        m >> fun (result, resource) -> (k result) resource

    member x.Combine (r1, r2) : MachineMonad = 
        x.Bind (r1, fun () -> r2)

    member internal x.Set (r, lens, value) = 
        x.Bind ((fun res -> (), setPL lens value res), fun _ -> x.ReturnFrom r)

[<AutoOpen>]
module Expression =

    let machine = MachineMonadBuilder ()

[<AutoOpen>]
module ActionOperations =

    type MachineMonadBuilder with

        [<CustomOperation (A.Delete, MaintainsVariableSpaceUsingBind = true)>]
        member x.DoDelete (r, f) = x.Set (r, Definition.actionsPLens A.Delete, f)

        [<CustomOperation (A.Patch, MaintainsVariableSpaceUsingBind = true)>]
        member x.DoPatch (r, f) = x.Set (r, Definition.actionsPLens A.Patch, f)

        [<CustomOperation (A.Post, MaintainsVariableSpaceUsingBind = true)>]
        member x.DoPost (r, f) = x.Set (r, Definition.actionsPLens A.Post, f)

        [<CustomOperation (A.Put, MaintainsVariableSpaceUsingBind = true)>]
        member x.DoPut (r, f) = x.Set (r, Definition.actionsPLens A.Put, f)

