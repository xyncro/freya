[<AutoOpen>]
module Freya.Recorder.Integration

open Freya.Core
open Freya.Core.Operators

(* Execution *)

let initializeRecord =
    setPLM requestIdPLens =<< (asyncM init =<< returnM ())

let updateRecord f =
    Option.iter (fun id -> update id f) <!> getPLM requestIdPLens

(* Inspection *)

let listRecords =
    asyncM list =<< returnM ()

let getRecord id =
    asyncM read =<< returnM id