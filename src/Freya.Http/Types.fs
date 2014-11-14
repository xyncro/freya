[<AutoOpen>]
module Freya.Http.Types

open System
open System.Globalization

(*  RFC 7230

    Types implemented to mirror the specification of HTTP semantics as defined
    in RFC 7230. See [http://tools.ietf.org/html/rfc7230]. *)

(*  Section 3 *)

/// Type representing the Method part of the Request Line of an HTTP message.
/// Standard semantic methods are represented directly as cases, custom methods 
/// will be represented by the "Custom" case.
/// See [http://tools.ietf.org/html/rfc7230#section-3.1] for details of the
/// basic semantics and [http://tools.ietf.org/html/rfc7231] for standards-defined
/// semantic methods.
type Method =
    | DELETE 
    | HEAD 
    | GET 
    | OPTIONS 
    | PATCH 
    | POST 
    | PUT 
    | TRACE 
    | Custom of string

/// Type representing the HTTP Version part of the Request Line of an HTTP message.
/// 1.0 and 1.1 versions are represented via the HTTP case, any other values will
/// be represented by the "Custom" case.
/// See [http://tools.ietf.org/html/rfc7230#section-3.1].
type HttpVersion =
    | HTTP of float 
    | Custom of string

type ContentLength =
    | ContentLength of int

/// Type representing the Transfer-Encoding header for an HTTP request.
/// See [http://tools.ietf.org/html/rfc7230#section-3.3.1].
type TransferEncoding =
    string // TODO: TransferEncoding Type

(*  Section 4 *)

/// Type representing the TE header for an HTTP request.
/// See [http://tools.ietf.org/html/rfc7230#section-4.3].
type TE =
    string // TODO: TE Type

/// Type representing the Trailer header for an HTTP request.
/// See [http://tools.ietf.org/html/rfc7230#section-4.4].
type Trailer =
    string // TODO: Trailer Type

(*  Section 5 *)

/// Type representing the Host header of an HTTP message.
/// See [http://tools.ietf.org/html/rfc7230#section-5.4].
type Host = 
    string // TODO: Header Type

/// Type representing the Via header for an HTTP request.
/// See [http://tools.ietf.org/html/rfc7230#section-5.7.1].
type Via =
    string // TODO: Via Type

(*  Section 6 *)

/// Type representing the Connection header of an HTTP message.
/// See [http://tools.ietf.org/html/rfc7230#section-6.1].
type Connection =
    | Connection of ConnectionOption list

/// Type representing a single Connection Option as part of
/// a Connection header.
and ConnectionOption =
    | ConnectionOption of string

/// Type representing the Upgrade header for an HTTP request.
/// See [http://tools.ietf.org/html/rfc7230#section-6.7].
type Upgrade =
    string // TODOL: Upgrade Type

(*  RFC 7231

    Types implemented to mirror the specification of HTTP semantics as defined
    in RFC 7231. See [http://tools.ietf.org/html/rfc7231]. *)

type Parameters =
    Map<string, string>

type MediaType =
    | MediaType of Type * SubType * Parameters

and Type =
    | Type of string

and SubType =
    | SubType of string

(* Scheme
    
   Types representing the scheme of an HTTP request.
   See [http://tools.ietf.org/html/rfc7231] for details. *)

// TODO: Should this be here now?

type Scheme =
    | HTTP 
    | HTTPS 
    | Custom of string

(* Charsets *)

type CharsetSpec =
    | Charset of Charset
    | Any

and Charset =
    | Charset of string

(* Encodings *)

type EncodingSpec =
    | Encoding of Encoding
    | Identity
    | Any

and Encoding =
    | Encoding of string

(* Content-Type

   Taken from RFC 7231, Section 3.1.1.5 Content-Type
   [http://tools.ietf.org/html/rfc7231#section-3.1.1.5] *)

type ContentType =
    | ContentType of MediaType

(* Content-Encoding

   Taken from RFC 7231, Section 3.1.2.2. Content-Encoding
   [http://tools.ietf.org/html/rfc7231#section-3.1.2.2] *)

type ContentEncoding =
    | ContentEncoding of Encoding list

(* Expect

   Taken from RFC 7231, Section 5.1.1. Expect
   [http://tools.ietf.org/html/rfc7231#section-5.1.1] *)

type Expect =
    | Expect of Continue

and Continue =
    | Continue

(* Max-Forwards

   Taken from RFC 7231, Section 5.1.2. Max-Forwards
   [http://tools.ietf.org/html/rfc7231#section-5.1.2] *)

type MaxForwards =
    | MaxForwards of int

(* Accept

   Taken from RFC 7231, Section 5.3.2. Accept
   [http://tools.ietf.org/html/rfc7231#section-5.3.2] *)

/// Accept Header
type Accept =
    | Accept of AcceptableMedia list

and AcceptableMedia =
    { MediaRange: MediaRange
      Parameters: AcceptParameters option }

and MediaRange =
    | Closed of Type * SubType * Parameters
    | Partial of Type * Parameters
    | Open of Parameters

and AcceptParameters =
    { Weight: float
      Extensions: AcceptExtensions }

and AcceptExtensions =
    Map<string, string option>

(* Accept-Charset

   Taken from RFC 7231, Section 5.3.3. Accept-Charset
   [http://tools.ietf.org/html/rfc7231#section-5.3.3] *)

/// Accept-Charset Header
type AcceptCharset =
    | AcceptCharset of AcceptableCharset list

and AcceptableCharset =
    { Charset: CharsetSpec
      Weight: float option }

(* Accept-Encoding

   Taken from RFC 7231, Section 5.3.4. Accept-Encoding
   [http://tools.ietf.org/html/rfc7231#section-5.3.4] *)

/// Accept-Encoding Header
type AcceptEncoding =
    | AcceptEncoding of AcceptableEncoding list

and AcceptableEncoding =
    { Encoding: EncodingSpec
      Weight: float option }

(* Accept-Language

   Taken from RFC 7231, Section 5.3.5. Accept-Language
   [http://tools.ietf.org/html/rfc7231#section-5.3.5] *)

/// Accept-Language Header
type AcceptLanguage =
    | AcceptLanguage of AcceptableLanguage list

and AcceptableLanguage =
    { Language: CultureInfo
      Weight: float option }

(* Date

   Taken from RFC 7231, Section 7.1.1.2 Date
   [http://tools.ietf.org/html/rfc7231#section-7.1.1.2] *)

type Date =
    | Date of DateTime

(* Retry-After

   Taken from RFC 7231, Section 7.1.3. Retry-After
   [http://tools.ietf.org/html/rfc7231#section-7.1.3] *)

type RetryAfter =
    | Date of DateTime
    | Delay of int

(* Allow

   Taken from RFC 7231, Section 7.4.1 Allow
   [http://tools.ietf.org/html/rfc7231#section-7.4.1] *)

type Allow =
    | Allow of Method list

(* RFC 7232 *)

type LastModified =
    | LastModified of DateTime

(* Entity Tags *)

type EntityTag =
    | Strong of string
    | Weak of string

(* If-Match

   Taken from RFC 7232, Section 3.1, If-Match
   [http://tools.ietf.org/html/rfc7232#section-3.1] *)

/// If-Match Header
type IfMatch =
    | EntityTags of EntityTag list
    | Any

(* If-None-Match

   Taken from RFC 7232, Section 3.2, If-None-Match
   [http://tools.ietf.org/html/rfc7232#section-3.2] *)

/// If-None-Match Header
type IfNoneMatch =
    | EntityTags of EntityTag list
    | Any

type IfModifiedSince =
    | IfModifiedSince of DateTime

type IfUnmodifiedSince =
    | IfUnmodifiedSince of DateTime

(* RFC 7234 *)

type Age =
    | Age of TimeSpan

type Expires =
    | Expires of DateTime

(* RFC 7235 *)

(* Authorization
    
   Taken from RFC 7235, Section 4.2. Authorization
   [http://tools.ietf.org/html/rfc7235#section-4.2] *)

type Authorization =
    { Scheme: AuthorizationScheme }

and AuthorizationScheme =
    | AuthorizationScheme of string