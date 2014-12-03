module Freya.Typed.Tests.Negotiation

open NUnit.Framework
open Swensen.Unquote
open Freya.Machine
open Freya.Types.Http


[<Test>]
let ``negotiateAccept`` () =
    let available =
        [ MediaType.JSON
          MediaType.XML ]

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
        [ MediaType.JSON
          MediaType.XML ]

    negotiated2 =?
        [ MediaType.XML
          MediaType.JSON ]

    negotiated3 =? []

[<Test>]
let ``negotiateAcceptCharset`` () =
    let available =
        [ Charset.Unicode
          Charset.Iso88591 ]

    let requested1 =
        [ { Charset = CharsetRange.Charset (Charset.Unicode)
            Weight = Some 0.8 }
          { Charset = CharsetRange.Charset (Charset.Iso88591)
            Weight = Some 0.9 } ] |> AcceptCharset

    let requested2 =
        [ { Charset = CharsetRange.Charset (Charset.Unicode)
            Weight = None }
          { Charset = CharsetRange.Charset (Charset.Iso88591)
            Weight = Some 0.9 } ] |> AcceptCharset

    let negotiated1 = AcceptCharset.negotiate requested1 available
    let negotiated2 = AcceptCharset.negotiate requested2 available

    negotiated1 =? 
        [ Charset.Iso88591 
          Charset.Unicode ]

    negotiated2 =? 
        [ Charset.Unicode
          Charset.Iso88591 ]

[<Test>]
let ``negotiateAcceptEncoding`` () =
    let available =
        [ ContentCoding.GZip ]

    let requested1 =
        [ { Encoding = Coding (ContentCoding.GZip)
            Weight = Some 0.7 } ] |> AcceptEncoding

    let requested2 =
        [ { Encoding = Coding (ContentCoding.Compress)
            Weight = Some 0.7 } ] |> AcceptEncoding

    let negotiated1 = AcceptEncoding.negotiate requested1 available
    let negotiated2 = AcceptEncoding.negotiate requested2 available

    negotiated1 =? [ ContentCoding.GZip ]
    negotiated2 =? []

// TODO: Accept-Language Negotiation Tests!