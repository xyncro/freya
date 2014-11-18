[<AutoOpen>]
module Freya.Http.RFC3986

#nowarn "60"

open System.Net
open System.Net.Sockets
open FParsec

(* RFC 3986

   Types, parsers and formatters implemented to mirror the specification of 
   URI semantics as defined in RFC 3986.

   Taken from [http://tools.ietf.org/html/rfc3986] *)

(* Characters

   Taken from RFC 3986, Section 2 Characters
   See [http://tools.ietf.org/html/rfc3986#section-2] *)

let private unreserved =
    Set.unionMany [
        RFC5234.alpha
        RFC5234.digit
        set [ '-'; '.'; '_'; '~' ] ]

let private genDelims =
    set [ ':'; '/'; '?'; '#'; '['; ']'; '@' ]

let private subDelims =
    set [ '!'; '$'; '&'; '\''; '('; ')'; '*'; '+'; ','; ';'; '=' ]

let private reserved =
    Set.unionMany [
        genDelims
        subDelims ]

(* Scheme

   Taken from RFC 3986, Section 3.1 Scheme
   See [http://tools.ietf.org/html/rfc3986#section-3.1] *)

type Scheme =
    | Scheme of string

let private schemeF =
    function | Scheme x -> append x

let private schemeChars =
    Set.unionMany [
        RFC5234.alpha
        RFC5234.digit
        set [ '+'; '-'; '.' ] ]

let private schemeP =
    satisfy ((?>) RFC5234.alpha) .>>. manySatisfy ((?>) schemeChars)
    |>> ((fun (x, xs) -> sprintf "%c%s" x xs) >> Scheme)

type Scheme with

    static member Format =
        format schemeF

    static member TryParse =
        parseOption schemeP

    override x.ToString () =
        Scheme.Format x

(* Authority

   Taken from RFC 3986, Section 3.2 Authority
   See [http://tools.ietf.org/html/rfc3986#section-3.2] *)

type Authority =
    { Host: Host
      Port: Port option
      UserInfo: UserInfo option }

and Host =
    | IPv4 of IPAddress
    | IPv6 of IPAddress
    | RegName of string

and Port =
    | Port of int

and UserInfo =
    | UserInfo of string

(* Section 3.2.1 *)

let private userInfoChars =
    Set.unionMany [
        unreserved
        subDelims
        set [ ':' ] ]

let private userInfoF =
    function | UserInfo x -> append x >> append "@"

let private userInfoP =
    manySatisfy ((?>) userInfoChars) .>> skipChar '@' |>> UserInfo

(* Section 3.2.2 *)

(* Note: In this instance we use the built in IP Address type and parser
   as the standard is implemented fully and seems to handle all suitable cases.
   
   We also make a slight restriction for practicality at the moment on
   implementing IP-literal as an IPv6 specific type, discarding IPvFuture. As
   it stands, that's unlikely to be an issue, but could perhaps be revisited. *)

let private hostF =
    function | IPv4 x -> append (string x)
             | IPv6 x -> append "[" >> append (string x) >> append "]"
             | RegName x -> append x

let private ipv6Chars =
    Set.unionMany [
        RFC5234.hexdig
        set [ ':' ] ]

let private ipLiteralP =
    skipChar '[' >>. (many1Satisfy ((?>) ipv6Chars) >>= (fun x ->
        match IPAddress.TryParse x with
        | true, x when x.AddressFamily = AddressFamily.InterNetworkV6 -> preturn (IPv6 x)
        | _ -> pzero)) .>> skipChar ']'

let private ipv4Chars =
    Set.unionMany [
        RFC5234.digit
        set [ '.' ] ]

let private ipv4AddressP =
    many1Satisfy ((?>) ipv4Chars) >>= (fun x ->
        match IPAddress.TryParse x with
        | true, x when x.AddressFamily = AddressFamily.InterNetwork -> preturn (IPv4 x)
        | _ -> pzero)

let private regNameChars =
    Set.unionMany [
        unreserved
        subDelims ]

let private regNameP =
    manySatisfy ((?>) regNameChars) |>> RegName

let private hostP =
    choice [
        attempt ipLiteralP
        attempt ipv4AddressP
        regNameP ]

(* Section 3.2.3 *)

let private portF =
    function | Port x -> append ":" >> append (string x)

let private portP =
    skipChar ':' >>. puint32 |>> (int >> Port)

(* Section 3.2 *)

let private authorityP =
    opt (attempt userInfoP) .>>. hostP .>>. opt portP
    |>> fun ((user, host), port) ->
        { Host = host
          Port = port
          UserInfo = user }

type Authority with
    
    static member TryParse =
        parseOption authorityP