namespace Dyfrig.Routing


[<AutoOpen>]
module RoutingOperations =

    type RoutingMonadBuilder with

        // Registration

        [<CustomOperation ("register", MaintainsVariableSpaceUsingBind = true)>]
        member x.Register (r, path, app) = x.Update (r, add path app)
    
        // Utility

        [<CustomOperation ("including", MaintainsVariableSpaceUsingBind = true)>]
        member x.Including (r, routes) = x.Combine (r, routes)