[<AutoOpen>]
module Freya.Http.RFC7233

#nowarn "60"

open System
open FParsec

(* RFC 7233

   Types, parsers and formatters implemented to mirror the semantics of
   HTTP as defined in RFC 7233.
   
   See [http://tools.ietf.org/html/rfc7233] *)

(* If-Range

   Taken from RFC 7233, Section 3.2 If-Range
   See [http://tools.ietf.org/html/rfc7233#section-3.2] *)

type IfRange =
    | Date of DateTime
    | EntityTag of EntityTag

let private ifRangeF =
    function | Date x -> append (x.ToString "r")
             | EntityTag x -> entityTagF x

let private ifRangeP =
    (entityTagP |>> EntityTag) <|> (httpDateP |>> Date)

type IfRange with

    static member Format =
        Formatting.format ifRangeF

    static member TryParse =
        Parsing.parseP ifRangeP

    override x.ToString () =
        IfRange.Format x