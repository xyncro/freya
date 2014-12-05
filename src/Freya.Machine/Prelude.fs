[<AutoOpen>]
module internal Freya.Machine.Prelude

open System.Runtime.CompilerServices

(* Internals *)

[<assembly:InternalsVisibleTo ("Freya.Machine.Inspector")>]
do ()

(* List Extensions *)

[<RequireQualifiedAccess>]
module List =

    let tryMaxBy projection =
        function | [] -> None
                 | xs -> Some (List.maxBy projection xs)

(* Option Extensions *)

[<RequireQualifiedAccess>]
module Option =

    let getOrElse def =
        function | Some x -> x
                 | _ -> def

    let getOrElseOptionF f =
        function | Some x -> Some x
                 | _ -> f ()
