[<AutoOpen>]
module internal Freya.Router.Prelude

open System.Runtime.CompilerServices

(* Internals *)

[<assembly:InternalsVisibleTo ("Freya.Router.Inspector")>]
do ()

(* Functions *)

let inline flip f a b = 
    f b a