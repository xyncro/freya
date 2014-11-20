[<AutoOpen>]
module Freya.Typed.RFC5646

#nowarn "60"

open FParsec

(* RFC 5646

   Types, parsers and formatters implemented to mirror the specification of 
   Language Tag semantics as defined in RFC 5646.

   Taken from [http://tools.ietf.org/html/rfc5646] *)

(* Helpers *)

let private isAlpha =
    ((?>) RFC5234.alpha)

let private isDigit =
    ((?>) RFC5234.digit)

let private alphaP min max =
    manyMinMaxSatisfy min max isAlpha .>>? notFollowedBy (skipSatisfy isAlpha)

let private digitP min max =
    manyMinMaxSatisfy min max isDigit .>>? notFollowedBy (skipSatisfy isDigit)

(* Language *)

type Language =
    | Language of string * string list option

let private extP =
    skipChar '-' >>. alphaP 3 3

let private extLangP =
    choice [
        attempt (tuple3 extP extP extP) |>> fun (a, b, c) -> a :: b :: [ c ]
        attempt (tuple2 extP extP) |>> fun (a, b) -> a :: [ b ]
        extP |>> fun a -> [ a ] ]

let private languageP =
    choice [
        alphaP 2 3 .>>. opt (attempt extLangP) |>> Language
        alphaP 4 4 |>> (fun x -> Language (x, None))
        alphaP 5 8 |>> (fun x -> Language (x, None)) ]

type Language with

    static member Parse =
        parseExact languageP

    static member TryParse =
        parseOption languageP

(* Script *)

type Script =
    | Script of string

let private scriptP =
    skipChar '-' >>. alphaP 4 4 |>> Script

(* Region *)

type Region =
    | Region of string

let private regionP =
    skipChar '-' >>. (alphaP 2 2 <|> digitP 3 3) |>> Region

(* Language Tag *)

type LanguageTag =
    { Language: Language
      Script: Script option
      Region: Region option }

let private languageTagP =
    tuple3 languageP (opt scriptP) (opt regionP)
    |>> fun (language, script, region) ->
        { Language = language
          Script = script
          Region = region }

type LanguageTag with

    static member Parse =
        parseExact languageTagP

    static member TryParse =
        parseOption languageTagP