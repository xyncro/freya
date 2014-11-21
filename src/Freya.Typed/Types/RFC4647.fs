[<AutoOpen>]
module Freya.Typed.RFC4647

#nowarn "60"

open FParsec

(* RFC 4647

   Types, parsers and formatters implemented to mirror the specification of 
   Language Range semantics as defined in RFC 4647.

   Taken from [http://tools.ietf.org/html/rfc4647] *)

type LanguageRange =
    | Range of string list
    | Any

let internal languageRangeF =
    function | Range x -> join (append "-") append x
             | Any -> append "*"

let internal languageRangeP =
    choice [
        skipChar '*' >>% Any
        alphaP 1 8 .>>. many (skipChar '-' >>. alphaNumP 1 8) |>> (fun (x, xs) -> Range (x :: xs)) ]

type LanguageRange with

    static member Format =
        format languageRangeF

    static member Parse =
        parseExact languageRangeP

    static member TryParse =
        parseOption languageRangeP

    override x.ToString () =
        LanguageRange.Format x