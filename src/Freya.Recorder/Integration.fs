[<AutoOpen>]
module Freya.Recorder.Integration

open Freya.Core
open Freya.Core.Operators

(* Functions *)

let initR () =
    setPLM requestIdPLens =<< (asyncM init =<< returnM ())

let listR () =
    asyncM list =<< returnM ()

let getR id =
    asyncM read =<< returnM id

let modR f =
    Option.iter (fun id -> update id f) <!> getPLM requestIdPLens