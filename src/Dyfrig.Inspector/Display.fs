[<AutoOpen>]
module Dyfrig.Inspector.Interface

open System.Text
open Dyfrig.Core
open Dyfrig.Core.Operators
open Dyfrig.Http
open Dyfrig.Pipeline

let private render logs =
    owin {
        let body = Encoding.UTF8.GetBytes (string (Seq.toList logs))
        let length = Array.length body

        do! setPLM Response.Headers.contentLength (ContentLength length)
        do! setPLM Response.Headers.contentType (ContentType (MediaTypes.Text))
        do! modLM Response.body (fun b -> b.Write (body, 0, Array.length body); b) }

(* Pipeline *)

let internal display (storage: Storage) : Pipeline =
    render (storage.PostAndReply (fun c -> ReadAll (c))) *> halt