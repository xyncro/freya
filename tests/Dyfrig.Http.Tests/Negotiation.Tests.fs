module Dyfrig.Http.Tests.Negotiation

open NUnit.Framework
open Swensen.Unquote
open Dyfrig.Http


[<Test>]
let ``negotiateAccept`` =
    let available =
        [ { MediaType = MediaType (Type "application", SubType "json")
            Parameters = Map.empty }
          { MediaType = MediaType (Type "text", SubType "html")
            Parameters = Map.empty } ]

    let requested1 =
        [ { MediaRange = 
              { MediaRange = MediaRangeSpec.Closed (Type "application", SubType "json")
                Parameters = Map.empty }
            Weight = Some 0.8
            Parameters = Map.empty }
          { MediaRange = 
              { MediaRange = MediaRangeSpec.Partial (Type "text")
                Parameters = Map.empty }
            Weight = Some 0.5
            Parameters = Map.empty } ] |> Accept

    let requested2 =
        [ { MediaRange = 
              { MediaRange = MediaRangeSpec.Closed (Type "application", SubType "json")
                Parameters = Map.empty }
            Weight = Some 0.8
            Parameters = Map.empty }
          { MediaRange = 
              { MediaRange = MediaRangeSpec.Partial (Type "text")
                Parameters = Map.empty }
            Weight = Some 0.9
            Parameters = Map.empty } ] |> Accept

    let requested3 =
        [ { MediaRange = 
              { MediaRange = MediaRangeSpec.Open
                Parameters = Map.empty }
            Weight = Some 0.
            Parameters = Map.empty } ] |> Accept

    let negotiated1 = negotiateAccept available requested1
    let negotiated2 = negotiateAccept available requested2
    let negotiated3 = negotiateAccept available requested3
        
    negotiated1 =? 
        [ { MediaType = MediaType (Type "application", SubType "json")
            Parameters = Map.empty }
          { MediaType = MediaType (Type "text", SubType "html")
            Parameters = Map.empty } ]

    negotiated2 =?
        [ { MediaType = MediaType (Type "text", SubType "html")
            Parameters = Map.empty }
          { MediaType = MediaType (Type "application", SubType "json")
            Parameters = Map.empty } ]

    negotiated3 =? []

[<Test>]
let ``negotiateAcceptCharset`` () =
    let available =
        [ Charset "unicode-1-1"
          Charset "iso-8859-1" ]

    let requested1 =
        [ { Charset = CharsetSpec.Charset (Charset "unicode-1-1")
            Weight = Some 0.8 }
          { Charset = CharsetSpec.Charset (Charset "iso-8859-1")
            Weight = Some 0.9 } ] |> AcceptCharset

    let requested2 =
        [ { Charset = CharsetSpec.Charset (Charset "unicode-1-1")
            Weight = None }
          { Charset = CharsetSpec.Charset (Charset "iso-8859-1")
            Weight = Some 0.9 } ] |> AcceptCharset

    let negotiated1 = negotiateAcceptCharset available requested1
    let negotiated2 = negotiateAcceptCharset available requested2

    negotiated1 =? 
        [ Charset "iso-8859-1"  
          Charset "unicode-1-1" ]

    negotiated2 =? 
        [ Charset "unicode-1-1"
          Charset "iso-8859-1" ]

[<Test>]
let ``negotiateAcceptEncoding`` () =
    let available =
        [ Encoding.Encoding "gzip" ]

    let requested1 =
        [ { Encoding = EncodingSpec.Encoding (Encoding "gzip")
            Weight = Some 0.7 } ] |> AcceptEncoding

    let requested2 =
        [ { Encoding = EncodingSpec.Encoding (Encoding "compress")
            Weight = Some 0.7 } ] |> AcceptEncoding

    let negotiated1 = negotiateAcceptEncoding available requested1
    let negotiated2 = negotiateAcceptEncoding available requested2

    negotiated1 =? [ Encoding "gzip" ]
    negotiated2 =? []