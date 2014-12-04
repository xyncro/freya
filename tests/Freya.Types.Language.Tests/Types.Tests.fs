module Freya.Typed.Tests.RFC5646

open NUnit.Framework
open Freya.Types.Language
open Freya.Types.Tests

[<Test>]
let ``LanguageTag Formatting/Parsing`` () =

    (* Language *)

    let langTyped =
        { Language = Language ("de", None)
          Script = None
          Region = None
          Variant = Variant [] }

    let langString =
        "de"

    (* Language + Script *)

    let langScriptTyped =
        { Language = Language ("zh", None)
          Script = Some (Script "Hant")
          Region = None
          Variant = Variant [] }

    let langScriptString =
        "zh-Hant"

    (* Extended Language + Script + Region *)

    let langScriptRegionTyped =
        { Language = Language ("zh", Some [ "cmn" ])
          Script = Some (Script "Hans")
          Region = Some (Region "CN")
          Variant = Variant [] }

    let langScriptRegionString =
        "zh-cmn-Hans-CN"

    (* Language + Variant *)

    let langVariantTyped =
        { Language = Language ("sl", None)
          Script = None
          Region = None
          Variant = Variant [ "rozaj" ] }

    let langVariantString =
        "sl-rozaj"

    (* Language + Region + Variant *)

    let langRegionVariantTyped =
        { Language = Language ("de", None)
          Script = None
          Region = Some (Region "CH")
          Variant = Variant [ "1901" ] }

    let langRegionVariantString =
        "de-CH-1901"

    (* Language + Script + Region + Variant *)

    let langScriptRegionVariantTyped =
        { Language = Language ("hy", None)
          Script = Some (Script "Latn")
          Region = Some (Region "IT")
          Variant = Variant [ "arevela" ] }

    let langScriptRegionVariantString =
        "hy-Latn-IT-arevela"

    (* Language + Region *)

    let langRegionTyped =
        { Language = Language ("de", None)
          Script = None
          Region = Some (Region "DE")
          Variant = Variant [] }

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