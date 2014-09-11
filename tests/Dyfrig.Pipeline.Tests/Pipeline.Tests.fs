namespace Dyfrig.Pipeline.Tests

open System.Collections.Generic
open NUnit.Framework
open Swensen.Unquote
open Dyfrig.Core
open Dyfrig.Pipeline
open Dyfrig.Pipeline.Operators


[<AutoOpen>]
module Data =

    let env () =
        let env = Dictionary<string, obj> () :> IDictionary<string, obj>
        env.["o1"] <- false
        env.["o2"] <- false
        env


[<AutoOpen>]
module Helpers =
    
    let test f =
        Async.RunSynchronously (f (env ())) |> snd
     

module Composition =

    [<Test>]
    let ``pipeline executes both monads`` () =
        let o1 = modM (fun x -> x.["o1"] <- true; x)
        let o2 = modM (fun x -> x.["o2"] <- true; x)

        let env = test (o1 --> o2)

        unbox env.["o1"] =? true
        unbox env.["o2"] =? true

    [<Test>]
    let ``chain executes both monads if first returns continue`` () =
        let o1 =
            owin {
                do! modM (fun x -> x.["o1"] <- true; x)
                return Continue }

        let o2 = 
            owin {
                do! modM (fun x -> x.["o2"] <- true; x)
                return Continue }

        let env = test (o1 -?> o2)

        unbox env.["o1"] =? true
        unbox env.["o2"] =? true

    [<Test>]
    let ``chain executes only the first monad if first returns terminate`` () =
        let o1 =
            owin {
                do! modM (fun x -> x.["o1"] <- true; x)
                return Terminate }

        let o2 = 
            owin {
                do! modM (fun x -> x.["o2"] <- true; x)
                return Continue }

        let env = test (o1 -?> o2)

        unbox env.["o1"] =? true
        unbox env.["o2"] =? false