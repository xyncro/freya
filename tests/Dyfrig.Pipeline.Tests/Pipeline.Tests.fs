namespace Dyfrig.Pipeline.Tests

open System
open System.Collections.Generic
open NUnit.Framework
open Swensen.Unquote
open Dyfrig.Core
open Dyfrig.Core.Operators
open Dyfrig.Pipeline
open Dyfrig.Pipeline.Operators


[<AutoOpen>]
module Data =

    let env () =

        let data =
            dict [
                "o1", box false
                "o2", box false ]

        Dictionary<string, obj> (data, StringComparer.OrdinalIgnoreCase)


[<AutoOpen>]
module Helpers =
    
    let test f =
        Async.RunSynchronously (f (env ()))
     

module Composition =

    [<Test>]
    let ``pipeline executes both monads if first returns next`` () =
        let o1 = modM (fun x -> x.["o1"] <- true; x) >>= proceed
        let o2 = modM (fun x -> x.["o2"] <- true; x) >>= proceed

        let choice, env = test (o1 >?= o2)

        choice =? Proceed
        unbox env.["o1"] =? true
        unbox env.["o2"] =? true

    [<Test>]
    let ``pipeline executes only the first monad if first returns terminate`` () =
        let o1 = modM (fun x -> x.["o1"] <- true; x) >>= halt
        let o2 = modM (fun x -> x.["o2"] <- true; x) >>= proceed

        let choice, env = test (o1 >?= o2)

        choice =? Halt
        unbox env.["o1"] =? true
        unbox env.["o2"] =? false
