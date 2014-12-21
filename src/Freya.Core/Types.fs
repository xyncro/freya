[<AutoOpen>]
module Freya.Core.Types

open System
open System.Collections.Generic

(* Types *)

type FreyaState =
    { Environment: FreyaEnvironment
      Meta: FreyaMetaState }

and FreyaEnvironment =
    IDictionary<string, obj>

and FreyaMetaState =
    { Memos: Map<Guid, obj> }

(* Lenses *)

let internal environmentLens =
    (fun x -> x.Environment), 
    (fun e x -> { x with Environment = e })

let internal metaLens =
    (fun x -> x.Meta), 
    (fun m x -> { x with Meta = m })

let internal memosLens =
    (fun x -> x.Memos),
    (fun m x -> { x with Memos = m })