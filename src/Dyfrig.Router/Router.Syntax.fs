namespace Dyfrig.Router

[<AutoOpen>]
module Syntax =

    [<AutoOpen>]
    module Registration =

        type RoutesMonadBuilder with

            [<CustomOperation ("route", MaintainsVariableSpaceUsingBind = true)>]
            member x.Route (r, meth, path, pipeline) =
                x.Update (r, (fun x -> x @ [ { Method = meth; Path = path; Pipeline = pipeline } ]))

    
    [<AutoOpen>]
    module Utility =

        type RoutesMonadBuilder with

            [<CustomOperation ("including", MaintainsVariableSpaceUsingBind = true)>]
            member x.Including (r, routes) =
                x.Combine (r, routes)
