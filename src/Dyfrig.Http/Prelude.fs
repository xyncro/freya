[<AutoOpen>]
module internal Dyfrig.Http.Prelude


[<RequireQualifiedAccess>]
module Option =

    let getOrElse def =
        function | Some x -> x
                 | _ -> def