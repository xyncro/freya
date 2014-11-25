module Freya.Core.Utilities

open System

(* Memoization *)

let memoize<'a> (m: Freya<'a>) : Freya<'a> =
    let id = string (Guid.NewGuid ())

    freya {
        let! state = getM

        match state.TryGetValue id with
        | true, value -> 
            return (unbox<'a> value)
        | _ ->
            let! value = m

            state.[id] <- value
            return value }