[<AutoOpen>]
module Freya.Todo.Backend.Prelude

open System.IO
open System.Text
open Fleece
open Freya.Core
open Freya.Core.Operators
open Freya.Machine
open Freya.Types.Cors
open Freya.Types.Http
open Freya.Types.Language

// Utilities 

let tuple x y =
    x, y

// Presets

let corsOrigins : Freya<AccessControlAllowOriginRange> =
    returnM AccessControlAllowOriginRange.Any

let corsHeaders : Freya<string list> =
    returnM [ "accept"; "content-type" ]

let en : Freya<LanguageTag list> =
    returnM [ LanguageTag.Parse "en" ]

let json : Freya<MediaType list> =
    returnM [ MediaType.JSON ]

let utf8 : Freya<Charset list> =
    returnM [ Charset.UTF8 ]

// Defaults

let defaults =
    freyaMachine {
        charsetsSupported utf8
        corsHeadersSupported corsHeaders
        corsOriginsSupported corsOrigins
        languagesSupported en
        mediaTypesSupported json }

// Requested

let readStream (x: Stream) =
    use reader = new StreamReader (x)
    reader.ReadToEnd ()

let readBody () =
    readStream <!> getLM Request.body

let inline body () =
    (function | Choice1Of2 x -> Some x | _ -> None) <!> (parseJSON <!> readBody ())

// Response

let inline represent x =
    { Metadata =
        { Charset = Some Charset.UTF8
          Encodings = None
          MediaType = Some MediaType.JSON
          Languages = Some [ LanguageTag.Parse "en" ] }
      Data = (toJSON >> string >> Encoding.UTF8.GetBytes) x }