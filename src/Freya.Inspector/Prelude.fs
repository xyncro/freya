[<AutoOpen>]
module internal Freya.Inspector.Prelude

open System.Reflection
open System.Resources
open System.Text
open Freya.Core
open Freya.Core.Operators
open Freya.Types.Http
open Freya.Types.Language

(* Resource Access 

   Resources are stored in the separate resource assembly and loaded
   as UTF8 encoded binaries at runtime. *)

let private resourceAssembly =
    Assembly.GetExecutingAssembly ()

let resource key =
    use stream = resourceAssembly.GetManifestResourceStream (key)
    use reader = new System.IO.StreamReader (stream)

    Encoding.UTF8.GetBytes (reader.ReadToEnd ())

(* Defaults

   Convenience functions to return commonly used
   settings for resources, which helps to clean up common cases. *)

let en : Freya<LanguageTag list> =
    returnM [ LanguageTag.Parse "en" ]

let get : Freya<Method list> =
    returnM [ GET ]

let html : Freya<MediaType list> =
    returnM [ MediaType.HTML ]

let utf8 : Freya<Charset list> =
    returnM [ Charset.UTF8 ]