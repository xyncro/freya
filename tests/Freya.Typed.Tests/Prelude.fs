[<AutoOpen>]
module internal Freya.Typed.Tests.Prelude

open Swensen.Unquote

(* Test Helpers *)

let roundTrip<'a when 'a: equality> (iso: ('a -> string) * (string -> 'a)) =
    List.iter (fun (a, s) ->
        (fst iso) a =? s
        (snd iso) s =? a)