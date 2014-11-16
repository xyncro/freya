[<AutoOpen>]
module Freya.Http.RFC7232

#nowarn "60"

open System
open FParsec

(* RFC 7232

   Types, parsers and formatters implemented to mirror the semantics of
   HTTP as defined in RFC 7232.

   See [http://tools.ietf.org/html/rfc7232] *)

(* Last-Modified

   Taken from RFC 7232, Section 2.2 Last-Modified
   [http://tools.ietf.org/html/rfc7232#section-2.2] *)

type LastModified =
    | LastModified of DateTime

let private lastModifiedF =
    function | LastModified x -> append (x.ToString "r")

let private lastModifiedP =
    httpDateP |>> LastModified

type LastModified with

    static member Format =
        Formatting.format lastModifiedF

    static member TryParse =
        Parsing.parseP lastModifiedP

    override x.ToString () =
        LastModified.Format x

(* Entity Tag *)

type EntityTag =
    | Strong of string
    | Weak of string

let private entityTagF =
    function | Strong x -> appendf1 "\"{0}\"" x
             | Weak x -> appendf1 "W/\"{0}\"" x

(* TODO: This is a naive formulation of an entity tag and does not
   properly support the grammar, particularly weak references, which
   should be implemented ASAP *)

let private entityTagP =
    skipChar dquote >>. tokenP .>> skipChar dquote |>> Strong

type EntityTag with

    static member Format =
        Formatting.format entityTagF

    static member TryParse =
        Parsing.parseP entityTagP

    override x.ToString () =
        EntityTag.Format x

(* If-Match

   Taken from RFC 7232, Section 3.1, If-Match
   [http://tools.ietf.org/html/rfc7232#section-3.1] *)

type IfMatch =
    | EntityTags of EntityTag list
    | Any

let private ifMatchF =
    function | IfMatch.EntityTags x -> join entityTagF commaF x
             | IfMatch.Any -> append "*"

let private ifMatchP =
    choice [
        skipChar '*' >>% IfMatch.Any
        infixP commaP entityTagP |>> IfMatch.EntityTags ]

type IfMatch with

    static member Format =
        Formatting.format ifMatchF

    static member TryParse =
        Parsing.parseP ifMatchP

    override x.ToString () =
        IfMatch.Format x

(* If-None-Match

   Taken from RFC 7232, Section 3.2, If-None-Match
   [http://tools.ietf.org/html/rfc7232#section-3.2] *)

type IfNoneMatch =
    | EntityTags of EntityTag list
    | Any

let private ifNoneMatchF =
    function | IfNoneMatch.EntityTags x -> join entityTagF commaF x
             | IfNoneMatch.Any -> append "*"

let private ifNoneMatchP =
    choice [
        skipChar '*' >>% IfNoneMatch.Any
        infixP commaP entityTagP |>> IfNoneMatch.EntityTags ]

type IfNoneMatch with

    static member Format =
        Formatting.format ifNoneMatchF

    static member TryParse =
        Parsing.parseP ifNoneMatchP

    override x.ToString () =
        IfNoneMatch.Format x

(* If-Modified-Since

   Taken from RFC 7232, Section 3.3, If-Modified-Since
   [http://tools.ietf.org/html/rfc7232#section-3.3] *)

type IfModifiedSince =
    | IfModifiedSince of DateTime

let private ifModifiedSinceF =
    function | IfModifiedSince x -> append (x.ToString "r")

let private ifModifiedSinceP =
    httpDateP |>> IfModifiedSince

type IfModifiedSince with

    static member Format =
        Formatting.format ifModifiedSinceF

    static member TryParse =
        Parsing.parseP ifModifiedSinceP

    override x.ToString () =
        IfModifiedSince.Format x

(* If-Unmodified-Since

   Taken from RFC 7232, Section 3.4, If-Unmodified-Since
   [http://tools.ietf.org/html/rfc7232#section-3.4] *)

type IfUnmodifiedSince =
    | IfUnmodifiedSince of DateTime

let private ifUnmodifiedSinceF =
    function | IfUnmodifiedSince x -> append (x.ToString "r")

let private ifUnmodifiedSinceP =
    httpDateP |>> IfUnmodifiedSince

type IfUnmodifiedSince with

    static member Format =
        Formatting.format ifUnmodifiedSinceF

    static member TryParse =
        Parsing.parseP ifUnmodifiedSinceP

    override x.ToString () =
        IfUnmodifiedSince.Format x
