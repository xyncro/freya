namespace Dyfrig.Router

[<AutoOpen>]
module Operations =

    type RouterBuilder with

        // Registration

        [<CustomOperation ("register", MaintainsVariableSpaceUsingBind = true)>]
        member x.Register (r, path, app) = 
            x.Update (r, (fun x -> 
                x @ [ { Methods = None
                        Path = path
                        App = app } ]))
    
        // Utility

        [<CustomOperation ("including", MaintainsVariableSpaceUsingBind = true)>]
        member x.Including (r, routes) = 
            x.Combine (r, routes)