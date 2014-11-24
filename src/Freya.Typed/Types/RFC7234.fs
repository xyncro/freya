[<AutoOpen>]
module Freya.Typed.RFC7234

#nowarn "60"

open System
open FParsec

(* RFC 7234

   Types, parsers and formatters implemented to mirror the semantics of
   HTTP as defined in RFC 7234.

   See [http://tools.ietf.org/html/rfc7234] *)

(* Age

   Taken from RFC 7234 Section 5.1 Age
   See [http://tools.ietf.org/html/rfc7234#section-5.1] *)

type Age =
    | Age of TimeSpan

let private ageF =
    function | Age x -> append (string x.TotalSeconds)

let private ageP =
    puint32 |>> (float >> TimeSpan.FromSeconds >> Age)

type Age with

    static member Format =
        format ageF

    static member Parse =
        parseExact ageP

    static member TryParse =
        parseOption ageP

    override x.ToString () =
        Age.Format x

(* Cache-Control

   Taken from RFC 7234 Section 5.2 Cache-Control
   See [http://tools.ietf.org/html/rfc7234#section-5.2]

   Note that from a type system perspective we don't currently
   distinguish between cache-directives that are valid for
   requests/responses or both. This may be worth changing in future,
   but for now it should hopefully be clear enough when used. *)

type CacheControl =
    | CacheControl of CacheDirective list

and CacheDirective =
    | MaxAge of TimeSpan
    | MaxStale of TimeSpan
    | MinFresh of TimeSpan
    | MustRevalidate
    | NoCache
    | NoStore
    | NoTransform
    | OnlyIfCached
    | Private
    | ProxyRevalidate
    | Public
    | SMaxAge of TimeSpan
    | Custom of string * string option

(* Formatting *)

let private cacheDirectiveF =
    function | MaxAge x -> appendf1 "max-age={0}" (int x.TotalSeconds)
             | MaxStale x -> appendf1 "max-stale={0}" (int x.TotalSeconds)
             | MinFresh x -> appendf1 "min-fresh={0}" (int x.TotalSeconds)
             | MustRevalidate -> append "must-revalidate"
             | NoCache -> append "no-cache"
             | NoStore -> append "no-store"
             | NoTransform -> append "no-transform"
             | OnlyIfCached -> append "only-if-cached"
             | Private -> append "private"
             | ProxyRevalidate -> append "proxy-revalidate"
             | Public -> append "public"
             | SMaxAge x -> appendf1 "s-maxage={0}" x
             | Custom (x, Some y) -> appendf2 "{0}={2}" x y
             | Custom (x, _) -> append x

let private cacheControlF =
    function | CacheControl x -> join commaF cacheDirectiveF x

(* Parsing *)

// TODO: Custom Directive Parsing

let private cacheDirectiveP =
    choice [
        attempt (skipStringCI "max-age=" >>. puint32 |>> (float >> TimeSpan.FromSeconds >> MaxAge))
        attempt (skipStringCI "max-stake=" >>. puint32 |>> (float >> TimeSpan.FromSeconds >> MaxStale))
        attempt (skipStringCI "min-fresh=" >>. puint32 |>> (float >> TimeSpan.FromSeconds >> MinFresh))
        attempt (skipStringCI "must-revalidate" >>% MustRevalidate)
        attempt (skipStringCI "no-cache" >>% NoCache)
        attempt (skipStringCI "no-store" >>% NoStore)
        attempt (skipStringCI "no-transform" >>% NoTransform)
        attempt (skipStringCI "only-if-cached" >>% OnlyIfCached)
        attempt (skipStringCI "private" >>% Private)
        attempt (skipStringCI "proxy-revalidate" >>% ProxyRevalidate)
        attempt (skipStringCI "public" >>% Public)
        attempt (skipStringCI "s-maxage=" >>. puint32 |>> (float >> TimeSpan.FromSeconds >> SMaxAge)) ] 

let private cacheControlP =
    infix1P commaP cacheDirectiveP |>> CacheControl

(* Augmentation *)

type CacheControl with

    static member Format =
        format cacheControlF

    static member Parse =
        parseExact cacheControlP

    static member TryParse =
        parseOption cacheControlP

    override x.ToString () =
        CacheControl.Format x

(* Expires

   Taken from RFC 7234 Section 5.3 Expires
   See [http://tools.ietf.org/html/rfc7234#section-5.3] *)

type Expires =
    | Expires of DateTime

let private expiresF =
    function | Expires x -> append (x.ToString "r")

let private expiresP =
    httpDateP |>> Expires

type Expires with

    static member Format =
        format expiresF

    static member Parse =
        parseExact expiresP

    static member TryParse =
        parseOption expiresP

    override x.ToString () =
        Expires.Format x