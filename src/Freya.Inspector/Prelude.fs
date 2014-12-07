[<AutoOpen>]
module internal Freya.Inspector.Prelude

open System.IO
open System.Reflection
open System.Text
open Freya.Core
open Freya.Core.Operators
open Freya.Machine
open Freya.Types.Http
open Freya.Types.Language

(* Resources *)

let private resourceAssembly =
    Assembly.GetExecutingAssembly ()

let resource key =
    use stream = resourceAssembly.GetManifestResourceStream (key)
    use reader = new StreamReader (stream)

    Encoding.UTF8.GetBytes (reader.ReadToEnd ())

(* Defaults *)

let private en : Freya<LanguageTag list> =
    returnM [ LanguageTag.Parse "en" ]

let private get : Freya<Method list> =
    returnM [ GET ]

let private utf8 : Freya<Charset list> =
    returnM [ Charset.UTF8 ]

let defaults =
    freyaMachine {
        charsetsSupported utf8
        languagesSupported en
        methodsSupported get }

(* MediaTypes *)

let css : Freya<MediaType list> =
    returnM [ MediaType (Type "text", SubType "css", Map.empty) ]

let html : Freya<MediaType list> =
    returnM [ MediaType.HTML ]

(* Representation *)

let private firstNegotiatedOrElse def =
    function | Negotiated (x :: _) -> x
             | _ -> def

let represent n x =
    { Metadata =
        { Charset = Some (n.Charsets |> firstNegotiatedOrElse Charset.UTF8)
          Encodings = None
          MediaType = Some (n.MediaTypes |> firstNegotiatedOrElse MediaType.Text)
          Languages = Some [ n.Languages |> firstNegotiatedOrElse (LanguageTag.Parse "en") ] }
      Data = x }