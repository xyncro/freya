[<AutoOpen>]
module Freya.Typed.RFC7233

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
    | IfRange of IfRangeChoice

and IfRangeChoice =
    | Date of DateTime
    | EntityTag of EntityTag

let private ifRangeF =
    function | IfRange (Date x) -> append (x.ToString "r")
             | IfRange (EntityTag x) -> entityTagF x

let private ifRangeP =
    (entityTagP |>> (EntityTag >> IfRange)) <|> (httpDateP |>> (Date >> IfRange))

type IfRange with

    static member Format =
        format ifRangeF

    static member Parse =
        parseExact ifRangeP

    static member TryParse =
        parseOption ifRangeP

    override x.ToString () =
        IfRange.Format x