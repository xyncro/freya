[<AutoOpen>]
module Freya.Types.Prelude

open System.Runtime.CompilerServices
open Aether
open Freya.Core
open Freya.Core.Operators

(* Internals *)

[<assembly:InternalsVisibleTo ("Freya.Types.Http")>]
[<assembly:InternalsVisibleTo ("Freya.Types.Language")>]
[<assembly:InternalsVisibleTo ("Freya.Types.Uri")>]
do ()

(* Isomorphisms *)

let internal boxIso<'a> : Iso<obj, 'a> =
    unbox<'a>, box

(* Functions *)

/// Gets part of the OwinEnv using an Aether lens within an OWIN monad
let getLM l = 
    getL l <!> getM

/// Sets part of the OwinEnv using an Aether lens within an OWIN monad
let setLM l v = 
    setL l v |> modM

/// Modifies part of the OwinEnv using an Aether lens within an OWIN monad
let modLM l f = 
    modL l f |> modM

/// Gets part of the OwinEnv using a partial Aether lens within an OWIN monad
let getPLM l = 
    getPL l <!> getM

/// Sets part of the OwinEnv using a partial Aether lens within an OWIN monad
let setPLM l v = 
    setPL l v |> modM

/// Modifies part of the OwinEnv using a partial Aether lens within an OWIN monad
let modPLM l f = 
    modPL l f |> modM