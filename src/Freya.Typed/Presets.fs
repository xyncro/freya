﻿[<AutoOpen>]
module Freya.Typed.Presets


[<RequireQualifiedAccess>]
module Charsets =

    /// Convenience definition for "iso-8859-1"
    let Iso88591 =
        Charset "iso-8859-1"

    /// Convenience definition for "unicode-1-1"
    let Unicode =
        Charset "unicode-1-1"

    /// Convenience definition for "utf-8"
    let UTF8 =
        Charset "utf-8"


[<RequireQualifiedAccess>]
module ContentCodings =

    /// Convenience definition for "deflate"
    let Compress =
        ContentCoding "compress"

    /// Convenience definition for "deflate"
    let Deflate =
        ContentCoding "deflate"

    /// Convenience definition for "gzip"
    let GZip =
        ContentCoding "gzip"


[<RequireQualifiedAccess>]
module MediaTypes =

    /// Convenience definition for "application/json" without extra parameters
    let JSON =
        MediaType (Type "application", SubType "json", Map.empty)

    /// Convenience definition for "text/plain" without extra parameters
    let Text =
        MediaType (Type "text", SubType "plain", Map.empty)

    /// Convenience definition for "application/xml" without extra parameters
    let XML =
        MediaType (Type "application", SubType "xml", Map.empty)