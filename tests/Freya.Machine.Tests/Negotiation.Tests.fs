module Freya.Typed.Tests.Negotiation

open NUnit.Framework
open Swensen.Unquote
open Freya.Machine
open Freya.Types.Http
open Freya.Types.Language

[<Test>]
let ``MediaType Negotiation`` () =
    let supported =
        [ MediaType.JSON
          MediaType.XML ]

    let requested1 =
        Accept.Parse "application/json;q=0.8,application/*;q=0.5"

    let requested2 =
        Accept.Parse "application/json;q=0.8,application/*;q=0.9"

    let requested3 =
        Accept.Parse "*/*;q=0"

    let negotiated1 = Accept.negotiate (Some supported, Some requested1)
    let negotiated2 = Accept.negotiate (Some supported, Some requested2)
    let negotiated3 = Accept.negotiate (Some supported, Some requested3)
        
    negotiated1 =? 
        Some ([ MediaType.JSON
                MediaType.XML ])

    negotiated2 =?
        Some ([ MediaType.XML
                MediaType.JSON ])

    negotiated3 =? 
        Some ([])

[<Test>]
let ``Charset Negotiation`` () =
    let supported =
        [ Charset.Unicode
          Charset.Iso88591 ]

    let requested1 =
        AcceptCharset.Parse "unicode-1-1;q=0.8,iso-8859-1;q=0.9"

    let requested2 =
        AcceptCharset.Parse "unicode-1-1,iso-8859-1;q=0.9"

    let negotiated1 = AcceptCharset.negotiate (Some supported, Some requested1)
    let negotiated2 = AcceptCharset.negotiate (Some supported, Some requested2)

    negotiated1 =? 
        Some ([ Charset.Iso88591 
                Charset.Unicode ])

    negotiated2 =? 
        Some ([ Charset.Unicode
                Charset.Iso88591 ])

[<Test>]
let ``Encoding Negotiation`` () =
    let supported =
        [ ContentCoding.GZip
          ContentCoding.Compress ]

    let requested1 =
        AcceptEncoding.Parse "gzip;q=0.7"

    let requested2 =
        AcceptEncoding.Parse "deflate;q=0.7"

    let negotiated1 = AcceptEncoding.negotiate (Some supported, Some requested1)
    let negotiated2 = AcceptEncoding.negotiate (Some supported, Some requested2)

    negotiated1 =? Some ([ ContentCoding.GZip ])
    negotiated2 =? Some ([])

[<Test>]
let ``Language Negotiation`` () =
    let supported =
        [ LanguageTag.Parse "en-GB"
          LanguageTag.Parse "fr"
          LanguageTag.Parse "en" ]

    let requested1 =
        AcceptLanguage.Parse "en-GB,en-US;q=0.8,en;q=0.6"

    let negotiated1 = AcceptLanguage.negotiate (Some supported, Some requested1)

    negotiated1 =? Some ([ LanguageTag.Parse "en-GB"
                           LanguageTag.Parse "en" ])