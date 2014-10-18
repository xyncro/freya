[<AutoOpen>]
module Dyfrig.Http.Types

open System.Globalization

(* RFC 7231 *)

(* Method
    
   Types representing the method of an HTTP request.
   See [http://tools.ietf.org/html/rfc7231] for details. *)

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

(* Protocol
    
   Types representing the protocol of an HTTP request.
   See [http://tools.ietf.org/html/rfc7231] for details. *)

type Protocol =
    | HTTP of float 
    | Custom of string

(* Scheme
    
   Types representing the scheme of an HTTP request.
   See [http://tools.ietf.org/html/rfc7231] for details. *)

type Scheme =
    | HTTP 
    | HTTPS 
    | Custom of string

(* Media Types/Ranges *)

type Type =
    | Type of string

and SubType =
    | SubType of string

type MediaRange =
    { MediaRange: MediaRangeSpec
      Parameters: Map<string, string> }

and MediaRangeSpec =
    | Closed of Type * SubType
    | Partial of Type
    | Open

type MediaType =
    { MediaType: MediaTypeSpec
      Parameters: Map<string, string> }

and MediaTypeSpec =
    | MediaType of Type * SubType

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

(* Accept

   Taken from RFC 7231, Section 5.3.2. Accept
   [http://tools.ietf.org/html/rfc7231#section-5.3.2] *)

type Accept =
    { MediaRange: MediaRange
      Weight: float option
      Parameters: Map<string, string option> }

(* Accept-Charset

   Taken from RFC 7231, Section 5.3.3. Accept-Charset
   [http://tools.ietf.org/html/rfc7231#section-5.3.3] *)

type AcceptCharset =
    { Charset: CharsetSpec
      Weight: float option }

(* Accept-Encoding

   Taken from RFC 7231, Section 5.3.4. Accept-Encoding
   [http://tools.ietf.org/html/rfc7231#section-5.3.4] *)

type AcceptEncoding =
    { Encoding: EncodingSpec
      Weight: float option }

(* Accept-Language

   Taken from RFC 7231, Section 5.3.5. Accept-Language
   [http://tools.ietf.org/html/rfc7231#section-5.3.5] *)

type AcceptLanguage =
    { Language: CultureInfo
      Weight: float option }

(* RFC 7232 *)

(* Entity Tags *)

type EntityTag =
    | Strong of string
    | Weak of string

(* If-Match

   Taken from RFC 7232, Section 3.1, If-Match
   [http://tools.ietf.org/html/rfc7232#section-3.1] *)

type IfMatch =
    | EntityTags of EntityTag list
    | Any

(* If-None-Match

   Taken from RFC 7232, Section 3.2, If-None-Match
   [http://tools.ietf.org/html/rfc7232#section-3.2] *)

type IfNoneMatch =
    | EntityTags of EntityTag list
    | Any

(* RFC 7235 *)

(* Authorization
    
   Taken from RFC 7235, Section 4.2. Authorization
   [http://tools.ietf.org/html/rfc7235#section-4.2] *)

type Authorization =
    { Scheme: AuthorizationScheme }

and AuthorizationScheme =
    | AuthorizationScheme of string