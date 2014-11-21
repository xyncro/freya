module Freya.Typed.Tests.RFC5646

open NUnit.Framework
open Swensen.Unquote
open Freya.Typed

[<Test>]
let ``LanguageTag Parsing`` () =

    let definitions =
        [ "de", 
          { Language = Language ("de", None)
            Script = None
            Region = None }
                  
          "zh-Hant", 
          { Language = Language ("zh", None)
            Script = Some (Script "Hant")
            Region = None }
                  
          "zh-cmn-Hans-CN",
          { Language = Language ("zh", Some [ "cmn" ])
            Script = Some (Script "Hans")
            Region = Some (Region "CN") } ]

    definitions
    |> List.iter (fun (s, p) -> LanguageTag.TryParse s =? Some p)
