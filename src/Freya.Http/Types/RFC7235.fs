[<AutoOpen>]
module Freya.Http.RFC7235

#nowarn "60"

open System
open FParsec

(* RFC 7235

   Types, parsers and formatters implemented to mirror the semantics of
   HTTP as defined in RFC 7235.
   
   See [http://tools.ietf.org/html/rfc7235] *)

(* Access Authentication Framework
    
   Taken from RFC 7235, Section 2. Access Authentication Framework
   [http://tools.ietf.org/html/rfc7235#section-2] *)

type Credentials =
    | Token of AuthorizationScheme * string
    | Parameters of AuthorizationScheme * Map<string, string>

and AuthorizationScheme =
    | AuthorizationScheme of string

(* Formatting *)

let private authorizationSchemeF =
    function | AuthorizationScheme x -> append x

let private credentialsF =
    function | Token (x, y) -> authorizationSchemeF x >> append y
             | Parameters (x, y) -> authorizationSchemeF x >> parametersF y

(* Parsing *)

let private authorizationSchemeP =
    tokenP

let private authorizationParameterP =
    tokenP .>> bwsP .>> skipChar '=' .>>. (tokenP <|> quotedStringP)

// TODO: Parsers 

(* Authorization
    
   Taken from RFC 7235, Section 4.2. Authorization
   [http://tools.ietf.org/html/rfc7235#section-4.2] *)

type Authorization =
    | Authorization of Credentials list

let private authorizationF =
    function | Authorization x -> join commaF credentialsF x

// TODO: Parser

type Authorization with

    static member Format =
        format authorizationF

    override x.ToString () =
        Authorization.Format x