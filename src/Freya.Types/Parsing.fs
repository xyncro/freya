[<AutoOpen>]
module internal Freya.Types.Parsing

open FParsec

(* Parsing *)

let parseExact p s =
    match run p s with
    | Success (x, _, _) -> x
    | Failure (e, _, _) -> failwith e

let parseOption p s =
    match run p s with
    | Success (x, _, _) -> Some x
    | Failure (_, _, _) -> None

(* Helpers *)

let charRange x y =
    set (List.map char [ x .. y ])

let (?>) xs x =
    Set.contains x xs

(* Common *)

let ampersandP : Parser<unit, unit> =
    skipChar '&'

let commaP : Parser<unit, unit> =
    skipChar ','

let semicolonP : Parser<unit, unit> =
    skipChar ';'

let slashP : Parser<unit, unit> =
    skipChar '/'

let spaceP : Parser<unit, unit> =
    skipChar ' '