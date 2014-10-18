[<AutoOpen>]
module Dyfrig.Http.Presets


[<RequireQualifiedAccess>]
module Charsets =

    /// Convenience definition for "unicode-1-1"
    let Unicode =
        Charset "unicode-1-1"


[<RequireQualifiedAccess>]
module Encodings =

    /// Convenience definition for "deflate"
    let Deflate =
        Encoding "deflate"

    /// Convenience definition for "gzip"
    let GZip =
        Encoding "gzip"


[<RequireQualifiedAccess>]
module MediaTypes =

    /// Convenience definition for "application/json" without extra parameters
    let JSON =
        { MediaType = MediaType (Type "application", SubType "json")
          Parameters = Map.empty }

    /// Convenience definition for "text/plain" without extra parameters
    let Text =
        { MediaType = MediaType (Type "text", SubType "plain")
          Parameters = Map.empty }

    /// Convenience definition for "application/xml" without extra parameters
    let XML =
        { MediaType = MediaType (Type "application", SubType "xml")
          Parameters = Map.empty }