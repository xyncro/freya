[<AutoOpen>]
module internal Freya.Inspector.Prelude

open System.IO
open System.Reflection
open System.Text
open Fleece
open Freya.Core
open Freya.Core.Operators
open Freya.Machine
open Freya.Types.Http
open Freya.Types.Language

(* Presets

   Useful shorthand for commonly used properties/defaults
   of Machine resources, to make definitions more concise. *)

(* Charsets *)

let utf8 : Freya<Charset list> =
    returnM [ Charset.UTF8 ]

(* Languages *)

let en : Freya<LanguageTag list> =
    returnM [ LanguageTag.Parse "en" ]

(* MediaTypes *)

let css : Freya<MediaType list> =
    returnM [ MediaType (Type "text", SubType "css", Map.empty) ]

let html : Freya<MediaType list> =
    returnM [ MediaType.HTML ]

let json : Freya<MediaType list> =
    returnM [ MediaType.JSON ]

(* Defaults *)

let defaults =
    freyaMachine {
        charsetsSupported utf8
        languagesSupported en
        mediaTypesSupported json }

(* Functions

   Support functions for various aspects of Machine resource
   fulfilment such as reading static resources from embedded assembly
   resources, and negotiating the correct form for representations,
   including JSON serialization when appropriate. *)

(* Resources *)

let private resourceAssembly =
    Assembly.GetExecutingAssembly ()

let resource key =
    use stream = resourceAssembly.GetManifestResourceStream (key)
    use reader = new StreamReader (stream)

    Encoding.UTF8.GetBytes (reader.ReadToEnd ())

(* Representation *)

let private firstNegotiatedOrElse def =
    function | Negotiated (x :: _) -> x
             | _ -> def

let private encode =
    string >> Encoding.UTF8.GetBytes

let represent n x =
    { Metadata =
        { Charset = Some (n.Charsets |> firstNegotiatedOrElse Charset.UTF8)
          Encodings = None
          MediaType = Some (n.MediaTypes |> firstNegotiatedOrElse MediaType.Text)
          Languages = Some [ n.Languages |> firstNegotiatedOrElse (LanguageTag.Parse "en") ] }
      Data = x }

let representJSON x =
    { Metadata =
        { Charset = Some Charset.UTF8
          Encodings = None
          MediaType = Some MediaType.JSON
          Languages = Some [ LanguageTag.Parse "en" ] }
      Data = encode x }