[<AutoOpen>]
module internal Dyfrig.Http.Prelude

open System.Runtime.CompilerServices

[<assembly:InternalsVisibleTo ("Dyfrig.Http.Tests")>]
do ()

[<RequireQualifiedAccess>]
module Option =

    let getOrElse def =
        function | Some x -> x
                 | _ -> def