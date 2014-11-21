[<AutoOpen>]
module Freya.Typed.RFC5646

#nowarn "60"

open FParsec

(* RFC 5646

   Types, parsers and formatters implemented to mirror the specification of 
   Language Tag semantics as defined in RFC 5646.

   Taken from [http://tools.ietf.org/html/rfc5646] *)

(* Note: The current implementation does not implement either private use
   tags or grandfathered tags. Contributions are welcome if this is a real
   world issue for anyone.

   In addition, the main implementation of a language tag does not implement
   the "extension" property, or the optional "privateuse" property. As the RFC,
   even in the list of example language tags, never produces an example of either
   of these in use they have been assumed to be of low importance for now.

   However, if someone does show a valid and even slightly common use case,
   they will be implemented. *)

(* Helpers *)

let private isAlpha =
    ((?>) RFC5234.alpha)

let private isDigit =
    ((?>) RFC5234.digit)

let private isAlphaNum x =
    (RFC5234.alpha ?> x || RFC5234.digit ?> x)

let private alphaP min max =
    manyMinMaxSatisfy min max isAlpha .>>? notFollowedBy (skipSatisfy isAlpha)

let private digitP min max =
    manyMinMaxSatisfy min max isDigit .>>? notFollowedBy (skipSatisfy isDigit)

let private alphaNumP min max =
    manyMinMaxSatisfy min max isAlphaNum .>>? notFollowedBy (skipSatisfy isAlphaNum)

(* Language *)

type Language =
    | Language of string * string list option

let private extF =
    function | x -> append "-" >> append x

let private extLangF =
    function | xs -> join id extF xs

let private languageF =
    function | Language (x, Some e) -> append x >> extLangF e
             | Language (x, _) -> append x 

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

    static member Format =
        format languageF

    static member Parse =
        parseExact languageP

    static member TryParse =
        parseOption languageP

    override x.ToString () =
        Language.Format x

(* Script *)

type Script =
    | Script of string

let private scriptF =
    function | Script x -> append "-" >> append x 

let private scriptP =
    skipChar '-' >>. alphaP 4 4 |>> Script

(* Region *)

type Region =
    | Region of string

let private regionF =
    function | Region x -> append "-" >> append x

let private regionP =
    skipChar '-' >>. (alphaP 2 2 <|> digitP 3 3) |>> Region

(* Variant *)

type Variant =
    | Variant of string list

let private varF =
    function | x -> append "-" >> append x

let private variantF =
    function | Variant xs -> join id varF xs

let private alphaPrefixVariantP =
    alphaNumP 5 8

let private digitPrefixVariantP =
    satisfy isDigit .>>. alphaNumP 3 3 |>> fun (c, s) -> sprintf "%c%s" c s

let private varP =
    skipChar '-' >>. (alphaPrefixVariantP <|> digitPrefixVariantP)

let private variantP =
    many varP |>> Variant

(* Language Tag *)

type LanguageTag =
    { Language: Language
      Script: Script option
      Region: Region option
      Variant: Variant }

let private languageTagF =
    function | { Language = language
                 Script = script
                 Region = region
                 Variant = variant } ->
                 let formatters =
                    [ languageF language
                      (function | Some x -> scriptF x | _ -> id) script
                      (function | Some x -> regionF x | _ -> id) region
                      variantF variant ]

                 fun b -> List.fold (fun b f -> f b) b formatters

let private languageTagP =
    tuple4 languageP (opt (attempt scriptP)) (opt (attempt regionP)) (variantP)
    |>> fun (language, script, region, variant) ->
        { Language = language
          Script = script
          Region = region
          Variant = variant }

type LanguageTag with

    static member Format =
        format languageTagF

    static member Parse =
        parseExact languageTagP

    static member TryParse =
        parseOption languageTagP

    override x.ToString () =
        LanguageTag.Format x