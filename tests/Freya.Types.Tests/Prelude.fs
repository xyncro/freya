[<AutoOpen>]
module internal Freya.Types.Tests.Prelude

open System.Runtime.CompilerServices
open Swensen.Unquote

(* Internals *)

[<assembly:InternalsVisibleTo ("Freya.Types.Http.Tests")>]
[<assembly:InternalsVisibleTo ("Freya.Types.Language.Tests")>]
[<assembly:InternalsVisibleTo ("Freya.Types.Uri.Tests")>]
do ()

(* Helpers *)

let roundTrip<'a when 'a: equality> (iso: ('a -> string) * (string -> 'a)) =
    List.iter (fun (a, s) ->
        (fst iso) a =? s
        (snd iso) s =? a)