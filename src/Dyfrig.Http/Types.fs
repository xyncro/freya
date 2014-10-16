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

(* Accept

   Taken from RFC 7231, Section 5.3.2. Accept
   [http://tools.ietf.org/html/rfc7231#section-5.3.2] *)

type Accept =
    { MediaRange: MediaRange
      MediaRangeParameters: Map<string, string>
      ExtensionParameters: Map<string, string option>
      Weight: float option }

and MediaRange =
    | Specified of SpecifiedMediaRange
    | Partial of PartialMediaRange

and SpecifiedMediaRange =
    | Closed of MediaType * MediaSubType

and PartialMediaRange =
    | Partial of MediaType
    | Open

and MediaType =
    | MediaType of string

and MediaSubType =
    | MediaSubType of string

(* Accept-Charset

   Taken from RFC 7231, Section 5.3.3. Accept-Charset
   [http://tools.ietf.org/html/rfc7231#section-5.3.3] *)

type AcceptCharset =
    { Charset: Charset
      Weight: float option }

and Charset =
    | Specified of SpecifiedCharset
    | Any

and SpecifiedCharset =
    | Named of string

(* Accept-Encoding

   Taken from RFC 7231, Section 5.3.4. Accept-Encoding
   [http://tools.ietf.org/html/rfc7231#section-5.3.4] *)

type AcceptEncoding =
    { Encoding: Encoding
      Weight: float option }

and Encoding =
    | Specified of SpecifiedEncoding
    | Any

and SpecifiedEncoding =
    | Named of string
    | Identity

(* Accept-Language

   Taken from RFC 7231, Section 5.3.5. Accept-Language
   [http://tools.ietf.org/html/rfc7231#section-5.3.5] *)

type AcceptLanguage =
    { Language: CultureInfo
      Weight: float option }

(* RFC 7232 *)

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