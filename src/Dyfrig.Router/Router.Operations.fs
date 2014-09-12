namespace Dyfrig.Router

[<AutoOpen>]
module Operations =

    [<AutoOpen>]
    module Registration =

        type RoutesMonadBuilder with

            [<CustomOperation ("register", MaintainsVariableSpaceUsingBind = true)>]
            member x.Register (r, meth, path, pipeline) =
                x.Update (r, (fun x -> x @ [ { Method = meth; Path = path; Pipeline = pipeline } ]))

    
    [<AutoOpen>]
    module Utility =

        type RoutesMonadBuilder with

            [<CustomOperation ("including", MaintainsVariableSpaceUsingBind = true)>]
            member x.Including (r, routes) = 
                x.Combine (r, routes)
