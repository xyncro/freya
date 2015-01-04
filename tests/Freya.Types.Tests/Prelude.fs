[<AutoOpen>]
module Freya.Types.Tests.Prelude

open Swensen.Unquote

(* Helpers *)

let roundTrip<'a when 'a: equality> (iso: ('a -> string) * (string -> 'a)) =
    List.iter (fun (a, s) ->
        (fst iso) a =? s
        (snd iso) s =? a)