[<AutoOpen>]
module Freya.Types.Prelude

open System.Runtime.CompilerServices
open Aether
open Freya.Core
open Freya.Core.Operators

(* Internals *)

[<assembly:InternalsVisibleTo ("Freya.Types.Cors")>]
[<assembly:InternalsVisibleTo ("Freya.Types.Http")>]
[<assembly:InternalsVisibleTo ("Freya.Types.Language")>]
[<assembly:InternalsVisibleTo ("Freya.Types.Uri")>]
do ()