module Freya.Pipeline.Tests.Pipeline

open NUnit.Framework
open Swensen.Unquote
open Freya.Core
open Freya.Core.Operators
open Freya.Pipeline
open Freya.Pipeline.Operators

(* Tests *)

[<Test>]
let ``pipeline executes both monads if first returns next`` () =
    let o1 = modM (fun x -> x.Environment.["o1"] <- true; x) *> next
    let o2 = modM (fun x -> x.Environment.["o2"] <- true; x) *> next

    let choice, env = run (o1 >?= o2)

    choice =? Next
    unbox env.Environment.["o1"] =? true
    unbox env.Environment.["o2"] =? true

[<Test>]
let ``pipeline executes only the first monad if first returns terminate`` () =
    let o1 = modM (fun x -> x.Environment.["o1"] <- true; x) *> halt
    let o2 = modM (fun x -> x.Environment.["o2"] <- true; x) *> next

    let choice, env = run (o1 >?= o2)

    choice =? Halt
    unbox env.Environment.["o1"] =? true
    unbox env.Environment.["o2"] =? false
