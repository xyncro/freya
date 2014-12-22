[<AutoOpen>]
module Freya.Core.Types

open System
open System.Collections.Generic

(* Environment *)

type FreyaEnvironment =
    IDictionary<string, obj>

(* State *)

type FreyaState =
    { Environment: FreyaEnvironment
      Meta: FreyaMetaState }

    static member internal EnvironmentLens =
        (fun x -> x.Environment), 
        (fun e x -> { x with Environment = e })

    static member internal MetaLens =
        (fun x -> x.Meta), 
        (fun m x -> { x with Meta = m })

and FreyaMetaState =
    { Memos: Map<Guid, obj> }

    static member internal MemosLens =
        (fun x -> x.Memos),
        (fun m x -> { x with Memos = m })

(* Monad *)

type Freya<'T> =
    FreyaState -> Async<'T * FreyaState>