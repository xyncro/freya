[<AutoOpen>]
module internal Dyfrig.Http.Prelude

open System.Runtime.CompilerServices

[<assembly:InternalsVisibleTo ("Dyfrig.Http.Tests")>]
do ()

[<RequireQualifiedAccess>]
module List =

    let tryMaxBy projection =
        function | [] -> None
                 | xs -> Some (List.maxBy projection xs)


[<RequireQualifiedAccess>]
module Option =

    let getOrElse def =
        function | Some x -> x
                 | _ -> def