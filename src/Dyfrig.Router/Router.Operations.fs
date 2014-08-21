namespace Dyfrig.Router

[<AutoOpen>]
module Operations =

    [<AutoOpen>]
    module Registration =

        type RouterMonadBuilder with

            [<CustomOperation ("register", MaintainsVariableSpaceUsingBind = true)>]
            member x.Register (r, path, app) = 
                x.Update (r, (fun x -> x @ [ { Methods = None; Path = path; App = app } ]))

    
    [<AutoOpen>]
    module Utility =

        type RouterMonadBuilder with

            [<CustomOperation ("including", MaintainsVariableSpaceUsingBind = true)>]
            member x.Including (r, routes) = 
                x.Combine (r, routes)
