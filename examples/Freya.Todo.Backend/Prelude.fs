[<AutoOpen>]
module Freya.Todo.Backend.Prelude

open System
open System.IO
open System.Text
open Fleece
open Freya.Core
open Freya.Core.Operators
open Freya.Machine
open Freya.Types.Cors
open Freya.Types.Http
open Freya.Types.Language

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

// Serialization

let inline toJSON x =
    toJSON x
    |> string 
    |> Encoding.UTF8.GetBytes

// Representation

let inline represent x =
    { Metadata =
        { Charset = Some Charset.UTF8
          Encodings = None
          MediaType = Some MediaType.JSON
          Languages = Some [ LanguageTag.Parse "en" ] }
      Data = toJSON x }

// Body

let readStream (s: Stream) =
    use reader = new StreamReader (s)
    reader.ReadToEnd ()

let readBody () =
        readStream 
    <!> getLM Request.body

let inline body () : Freya<'a option> =
        (function | Choice1Of2 x -> Some x
                  | _ -> None)
    <!> (parseJSON <!> readBody ())