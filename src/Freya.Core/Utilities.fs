[<AutoOpen>]
module Freya.Core.Utilities

open System

(* Memoization *)

let memoM<'a> (m: Freya<'a>) : Freya<'a> =
    let id = string (Guid.NewGuid ())

    freya {
        let! state = getM

        printfn "looking for value at id %A" id

        match state.TryGetValue id with
        | true, value ->
            printfn "got memoized value at id %A - %A" id value

            return (unbox<'a> value)
        | _ ->
            printfn "memoizing value at id %A" id

            let! value = m

            state.[id] <- value
            return value }