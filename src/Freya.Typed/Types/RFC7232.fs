﻿[<AutoOpen>]
module Freya.Typed.RFC7232

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
        format lastModifiedF

    static member Parse =
        parseExact lastModifiedP

    static member TryParse =
        parseOption lastModifiedP

    override x.ToString () =
        LastModified.Format x

(* ETag

   Taken from RFC 7232 Section 2.3 ETag
   [http://tools.ietf.org/html/rfc7232#section-2.3] *)

(* Entity Tag *)

type EntityTag =
    | Strong of string
    | Weak of string

let internal entityTagF =
    function | Strong x -> appendf1 "\"{0}\"" x
             | Weak x -> appendf1 "W/\"{0}\"" x

let private eTagChars =
    Set.unionMany [
        set [ char 0x21 ]
        charRange 0x23 0x7e
        obsText ]

let private opaqueTagP =
    skipChar RFC5234.dquote >>. manySatisfy ((?>) eTagChars) .>> skipChar RFC5234.dquote

let internal entityTagP =
    choice [
        attempt (skipString "W/" >>. opaqueTagP |>> Weak)
        opaqueTagP |>> Strong ]

(* ETag *)

type ETag =
    | ETag of EntityTag

let private eTagF =
    function | ETag x -> entityTagF x

let private eTagP =
    entityTagP |>> ETag

type ETag with

    static member Format =
        format eTagF

    static member Parse =
        parseExact eTagP

    static member TryParse =
        parseOption eTagP

    override x.ToString () =
        ETag.Format x

(* If-Match

   Taken from RFC 7232, Section 3.1, If-Match
   [http://tools.ietf.org/html/rfc7232#section-3.1] *)

type IfMatch =
    | IfMatch of IfMatchChoice

and IfMatchChoice =
    | EntityTags of EntityTag list
    | Any

let private ifMatchF =
    function | IfMatch (EntityTags x) -> join commaF entityTagF x
             | IfMatch (Any) -> append "*"

let private ifMatchP =
    choice [
        skipChar '*' >>% IfMatch (Any)
        infixP commaP entityTagP |>> (EntityTags >> IfMatch) ]

type IfMatch with

    static member Format =
        format ifMatchF

    static member Parse =
        parseExact ifMatchP

    static member TryParse =
        parseOption ifMatchP

    override x.ToString () =
        IfMatch.Format x

(* If-None-Match

   Taken from RFC 7232, Section 3.2, If-None-Match
   [http://tools.ietf.org/html/rfc7232#section-3.2] *)

type IfNoneMatch =
    | IfNoneMatch of IfNoneMatchChoice

and IfNoneMatchChoice =
    | EntityTags of EntityTag list
    | Any

let private ifNoneMatchF =
    function | IfNoneMatch (EntityTags x) -> join commaF entityTagF x
             | IfNoneMatch (Any) -> append "*"

let private ifNoneMatchP =
    choice [
        skipChar '*' >>% IfNoneMatch (Any)
        infixP commaP entityTagP |>> (EntityTags >> IfNoneMatch) ]

type IfNoneMatch with

    static member Format =
        format ifNoneMatchF

    static member Parse =
        parseExact ifNoneMatchP

    static member TryParse =
        parseOption ifNoneMatchP

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
        format ifModifiedSinceF

    static member Parse =
        parseExact ifModifiedSinceP

    static member TryParse =
        parseOption ifModifiedSinceP

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
        format ifUnmodifiedSinceF

    static member Parse =
        parseExact ifUnmodifiedSinceP

    static member TryParse =
        parseOption ifUnmodifiedSinceP

    override x.ToString () =
        IfUnmodifiedSince.Format x