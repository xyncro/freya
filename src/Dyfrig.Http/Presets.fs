[<AutoOpen>]
module Dyfrig.Http.Presets


[<RequireQualifiedAccess>]
module Charsets =

    let Unicode =
        Charset "unicode-1-1"


[<RequireQualifiedAccess>]
module Encodings =

    let Deflate =
        Encoding "deflate"

    let GZip =
        Encoding "gzip"


[<RequireQualifiedAccess>]
module MediaTypes =

    let JSON =
        { MediaType = MediaType (Type "application", SubType "json")
          Parameters = Map.empty }

    let XML =
        { MediaType = MediaType (Type "application", SubType "xml")
          Parameters = Map.empty }