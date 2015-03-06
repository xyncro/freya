module Freya.Types.Language.Tests

open NUnit.Framework
open Freya.Types.Language
open Freya.Types.Tests

[<Test>]
let ``LanguageTag Formatting/Parsing`` () =

    (* Language *)

    let langTyped =
        LanguageTag (
            Language ("de", None),
            None,
            None,
            Variant [])

    let langString =
        "de"

    (* Language + Script *)

    let langScriptTyped =
        LanguageTag (
            Language ("zh", None),
            Some (Script "Hant"),
            None,
            Variant [])

    let langScriptString =
        "zh-Hant"

    (* Extended Language + Script + Region *)

    let langScriptRegionTyped =
        LanguageTag (
            Language ("zh", Some [ "cmn" ]),
            Some (Script "Hans"),
            Some (Region "CN"),
            Variant [])

    let langScriptRegionString =
        "zh-cmn-Hans-CN"

    (* Language + Variant *)

    let langVariantTyped =
        LanguageTag (
            Language ("sl", None),
            None,
            None,
            Variant [ "rozaj" ])

    let langVariantString =
        "sl-rozaj"

    (* Language + Region + Variant *)

    let langRegionVariantTyped =
        LanguageTag (
            Language ("de", None),
            None,
            Some (Region "CH"),
            Variant [ "1901" ])

    let langRegionVariantString =
        "de-CH-1901"

    (* Language + Script + Region + Variant *)

    let langScriptRegionVariantTyped =
        LanguageTag (
            Language ("hy", None),
            Some (Script "Latn"),
            Some (Region "IT"),
            Variant [ "arevela" ])

    let langScriptRegionVariantString =
        "hy-Latn-IT-arevela"

    (* Language + Region *)

    let langRegionTyped =
        LanguageTag (
            Language ("de", None),
            None,
            Some (Region "DE"),
            Variant [])

    let langRegionString =
        "de-DE"

    (* Round trip *)

    roundTrip (LanguageTag.Format, LanguageTag.Parse) [
        langTyped,                    langString
        langScriptTyped,              langScriptString
        langScriptRegionTyped,        langScriptRegionString
        langVariantTyped,             langVariantString
        langRegionVariantTyped,       langRegionVariantString
        langScriptRegionVariantTyped, langScriptRegionVariantString
        langRegionTyped,              langRegionString ]
