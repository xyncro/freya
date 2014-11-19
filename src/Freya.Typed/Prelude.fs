[<AutoOpen>]
module internal Freya.Typed.Prelude

open System.Text
open Aether
open FParsec

(* Isomorphisms *)

let boxIso<'a> : Iso<obj, 'a> =
    unbox<'a>, box

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

(* Formatting *)

[<AutoOpen>]
module Formatting =

    (* Formatting *)

    let format (formatter: 'a -> StringBuilder -> StringBuilder) =
        fun a -> string (formatter a (StringBuilder ()))

    (* Helpers *)

    type Formatter<'a> =
        'a -> StringBuilder -> StringBuilder

    type Separator =
        StringBuilder -> StringBuilder

    let append (s: string) (b: StringBuilder) =
        b.Append s

    let appendf1 (s: string) (v1: obj) (b: StringBuilder) =
        b.AppendFormat (s, v1)

    let appendf2 (s: string) (v1: obj) (v2: obj) (b: StringBuilder) =
        b.AppendFormat (s, v1, v2)

    let join<'a> (s: Separator) (f: Formatter<'a>) =
        let rec join values (b: StringBuilder) =
            match values with
            | [] -> b
            | h :: [] -> f h b
            | h :: t -> (f h >> s >> join t) b

        join

    (* Common *)

    let ampersandF : Separator =
        append "&"

    let commaF : Separator =
        append ","

    let semicolonF : Separator =
        append ";"

    let slashF : Separator =
        append "/"


(* Parsing *)

[<AutoOpen>]
module Parsing =

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