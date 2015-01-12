[<AutoOpen>]
module Freya.Machine.Syntax

open Aether

type FreyaMachineBuilder with

    [<CustomOperation ("include", MaintainsVariableSpaceUsingBind = true)>]
    member x.Include (m, definition) = 
        x.Combine (m, definition)

    [<CustomOperation ("using", MaintainsVariableSpaceUsingBind = true)>]
    member x.Using (m, extension) =
        x.Map (m, modL MachineDefinition.ExtensionsLens (Set.add extension))