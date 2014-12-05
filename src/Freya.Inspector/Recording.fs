[<AutoOpen>]
module internal Freya.Inspector.Recording

open System
open Freya.Recorder
open Freya.Types.Http

(* Types *)

type FreyaRecord =
    { Id: Guid
      Timestamp: DateTime
      Request: FreyaRequestRecord }

and FreyaRequestRecord =
    { Method: Method
      Path: string }

(* Constructors *)

let freyaRecord meth path =
    { Id = Guid.NewGuid ()
      Timestamp = DateTime.UtcNow
      Request =
        { Method = meth
          Path = path } }

(* Functions *)

let initFreyaR meth path =
    setR "freya" (freyaRecord meth path)