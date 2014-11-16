[<AutoOpen>]
module Freya.Http.RFC7234

#nowarn "60"

open System
open FParsec

(* RFC 7234

   Types, parsers and formatters implemented to mirror the semantics of
   HTTP as defined in RFC 7234.
   
   See [http://tools.ietf.org/html/rfc7234] *)

(* Age

   Taken from RFC 7234, Section 5.1 Age
   See [http://tools.ietf.org/html/rfc7234#section-5.1] *)

type Age =
    | Age of TimeSpan

let private ageF =
    function | Age x -> append (string x.Seconds)

let private ageP : FParser<Age> =
    puint32 |>> (float >> TimeSpan.FromSeconds >> Age)

type Age with

    static member Format =
        Formatting.format ageF

    static member TryParse =
        Parsing.parseP ageP

    override x.ToString () =
        Age.Format x

(* Expires

   Taken from RFC 7234, Section 5.3 Expires
   See [http://tools.ietf.org/html/rfc7234#section-5.3] *)

type Expires =
    | Expires of DateTime

let expiresF =
    function | Expires x -> append (x.ToString "r")

let expiresP =
    httpDateP |>> Expires

type Expires with

    static member Format =
        Formatting.format expiresF

    static member TryParse =
        Parsing.parseP expiresP

    override x.ToString () =
        Expires.Format x