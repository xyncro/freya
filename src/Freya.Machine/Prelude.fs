[<AutoOpen>]
module internal Freya.Machine.Prelude

open System

(* Operators *)

let (==) s1 s2 =
    String.Equals (s1, s2, StringComparison.OrdinalIgnoreCase)

(* Functions *)

let inline flip f a b = 
    f b a

(* List Extensions *)

[<RequireQualifiedAccess>]
module List =

    let chooseMaxBy projection =
           List.map (fun x -> x, projection x)
        >> List.choose (function | (x, Some y) -> Some (x, y) | _ -> None)
        >> List.sortBy (fun (_, y) -> y)
        >> List.map fst
        >> function | [] -> None | x :: _ -> Some x

(* Option Extensions *)

[<RequireQualifiedAccess>]
module Option =

    let getOrElse def =
        function | Some x -> x
                 | _ -> def

    let getOrElseOptionF f =
        function | Some x -> Some x
                 | _ -> f ()
