[<AutoOpen>]
module Freya.Http.RFC7235

#nowarn "60"

open System
open FParsec

(* RFC 7235

   Types, parsers and formatters implemented to mirror the semantics of
   HTTP as defined in RFC 7235.
   
   See [http://tools.ietf.org/html/rfc7235] *)

(* Authorization
    
   Taken from RFC 7235, Section 4.2. Authorization
   [http://tools.ietf.org/html/rfc7235#section-4.2] *)

type Authorization =
    { Scheme: AuthorizationScheme }

and AuthorizationScheme =
    | AuthorizationScheme of string