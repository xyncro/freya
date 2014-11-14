[<AutoOpen>]
module internal Freya.Http.Prelude

(* List Module Extensions *)

[<RequireQualifiedAccess>]
module List =

    let tryMaxBy projection =
        function | [] -> None
                 | xs -> Some (List.maxBy projection xs)

(* Option Module Extensions *)

[<RequireQualifiedAccess>]
module Option =

    let getOrElse def =
        function | Some x -> x
                 | _ -> def