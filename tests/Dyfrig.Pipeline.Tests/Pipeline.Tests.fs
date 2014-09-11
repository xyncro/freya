namespace Dyfrig.Pipeline.Tests

open NUnit.Framework
open Swensen.Unquote
open Dyfrig.Core
open Dyfrig.Core.Operators
open Dyfrig.Pipeline
open Dyfrig.Pipeline.Operators


[<AutoOpen>]
module Data =

    let env () =
        dict [
            "o1", box false
            "o2", box false ]


[<AutoOpen>]
module Helpers =
    
    let test f =
        Async.RunSynchronously (f (env ()))
     

module Composition =

    [<Test>]
    let ``pipeline executes both monads if first returns next`` () =
        let o1 = modM (fun x -> x.["o1"] <- true; x) >>= next
        let o2 = modM (fun x -> x.["o2"] <- true; x) >>= next

        let choice, env = test (o1 >?= o2)

        choice =? Next
        unbox env.["o1"] =? true
        unbox env.["o2"] =? true

    [<Test>]
    let ``chain executes only the first monad if first returns terminate`` () =
        let o1 = modM (fun x -> x.["o1"] <- true; x) >>= terminate
        let o2 = modM (fun x -> x.["o2"] <- true; x) >>= next

        let choice, env = test (o1 >?= o2)

        choice =? Terminate
        unbox env.["o1"] =? true
        unbox env.["o2"] =? false
