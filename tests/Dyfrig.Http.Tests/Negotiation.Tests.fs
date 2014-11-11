module Dyfrig.Http.Tests.Negotiation

open NUnit.Framework
open Swensen.Unquote
open Dyfrig.Http


[<Test>]
let ``negotiateAccept`` =
    let available =
        [ MediaTypes.JSON
          MediaTypes.XML ]

    let requested1 =
        [ { MediaRange = MediaRange.Closed (Type "application", SubType "json", Map.empty)
            Parameters =
                Some { Weight = 0.8
                       Extensions = Map.empty } }
          { MediaRange = MediaRange.Partial (Type "application", Map.empty)
            Parameters = 
                Some { Weight = 0.5
                       Extensions = Map.empty } } ] |> Accept

    let requested2 =
        [ { MediaRange = MediaRange.Closed (Type "application", SubType "json", Map.empty)
            Parameters =
                Some { Weight = 0.8
                       Extensions = Map.empty } }
          { MediaRange = MediaRange.Partial (Type "application", Map.empty)
            Parameters = 
                Some { Weight = 0.9
                       Extensions = Map.empty } } ] |> Accept

    let requested3 =
        [ { MediaRange = MediaRange.Open Map.empty
            Parameters =
                Some { Weight = 0.0
                       Extensions = Map.empty } } ] |> Accept

    let negotiated1 = Accept.negotiate requested1 available
    let negotiated2 = Accept.negotiate requested2 available
    let negotiated3 = Accept.negotiate requested3 available
        
    negotiated1 =? 
        [ MediaTypes.JSON
          MediaTypes.XML ]

    negotiated2 =?
        [ MediaTypes.XML
          MediaTypes.JSON ]

    negotiated3 =? []

[<Test>]
let ``negotiateAcceptCharset`` () =
    let available =
        [ Charsets.Unicode
          Charsets.Iso88591 ]

    let requested1 =
        [ { Charset = CharsetSpec.Charset (Charsets.Unicode)
            Weight = Some 0.8 }
          { Charset = CharsetSpec.Charset (Charsets.Iso88591)
            Weight = Some 0.9 } ] |> AcceptCharset

    let requested2 =
        [ { Charset = CharsetSpec.Charset (Charsets.Unicode)
            Weight = None }
          { Charset = CharsetSpec.Charset (Charsets.Iso88591)
            Weight = Some 0.9 } ] |> AcceptCharset

    let negotiated1 = AcceptCharset.negotiate requested1 available
    let negotiated2 = AcceptCharset.negotiate requested2 available

    negotiated1 =? 
        [ Charsets.Iso88591 
          Charsets.Unicode ]

    negotiated2 =? 
        [ Charsets.Unicode
          Charsets.Iso88591 ]

[<Test>]
let ``negotiateAcceptEncoding`` () =
    let available =
        [ Encodings.GZip ]

    let requested1 =
        [ { Encoding = EncodingSpec.Encoding (Encodings.GZip)
            Weight = Some 0.7 } ] |> AcceptEncoding

    let requested2 =
        [ { Encoding = EncodingSpec.Encoding (Encodings.Compress)
            Weight = Some 0.7 } ] |> AcceptEncoding

    let negotiated1 = AcceptEncoding.negotiate requested1 available
    let negotiated2 = AcceptEncoding.negotiate requested2 available

    negotiated1 =? [ Encodings.GZip ]
    negotiated2 =? []