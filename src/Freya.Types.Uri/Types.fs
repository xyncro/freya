//----------------------------------------------------------------------------
//
// Copyright (c) 2014
//
//    Ryan Riley (@panesofglass) and Andrew Cherry (@kolektiv)
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//----------------------------------------------------------------------------

[<AutoOpen>]
module Freya.Types.Uri.Types

#nowarn "60"

open System.Net
open System.Net.Sockets
open FParsec
open Freya.Types

(* RFC 3986

   Types, parsers and formatters implemented to mirror the specification of 
   URI semantics as defined in RFC 3986.

   Taken from [http://tools.ietf.org/html/rfc3986] *)

(* Characters

   Taken from RFC 3986, Section 2 Characters
   See [http://tools.ietf.org/html/rfc3986#section-2] *)

let private unreserved =
    Set.unionMany [
        Grammar.alpha
        Grammar.digit
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

let internal schemeF =
    function | Scheme x -> append x

let private schemeChars =
    Set.unionMany [
        Grammar.alpha
        Grammar.digit
        set [ '+'; '-'; '.' ] ]

let internal schemeP =
    satisfy ((?>) Grammar.alpha) .>>. manySatisfy ((?>) schemeChars)
    |>> ((fun (x, xs) -> sprintf "%c%s" x xs) >> Scheme)

type Scheme with

    static member Format =
        format schemeF

    static member Parse =
        parseExact schemeP

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
    | Name of string

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

let internal hostF =
    function | IPv4 x -> append (string x)
             | IPv6 x -> append "[" >> append (string x) >> append "]"
             | Name x -> append x

let private ipv6Chars =
    Set.unionMany [
        Grammar.hexdig
        set [ ':' ] ]

let private ipLiteralP =
    skipChar '[' >>. (many1Satisfy ((?>) ipv6Chars) >>= (fun x ->
        match IPAddress.TryParse x with
        | true, x when x.AddressFamily = AddressFamily.InterNetworkV6 -> preturn (IPv6 x)
        | _ -> pzero)) .>> skipChar ']'

let private ipv4Chars =
    Set.unionMany [
        Grammar.digit
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
    manySatisfy ((?>) regNameChars) |>> Name

let internal hostP =
    choice [
        attempt ipLiteralP
        attempt ipv4AddressP
        regNameP ]

(* Section 3.2.3 *)

let internal portF =
    function | Port x -> append ":" >> append (string x)

let internal portP =
    skipChar ':' >>. puint32 |>> (int >> Port)

(* Section 3.2 *)

let private authorityF =
    function | { Host = h
                 Port = p
                 UserInfo = u } ->
                    let formatters =
                        [ (function | Some u -> userInfoF u | _ -> id) u
                          hostF h
                          (function | Some p -> portF p | _ -> id) p ]

                    fun b -> List.fold (fun b f -> f b)b formatters

let private authorityP =
    opt (attempt userInfoP) .>>. hostP .>>. opt portP
    |>> fun ((user, host), port) ->
        { Host = host
          Port = port
          UserInfo = user }

type Authority with

    static member Format =
        format authorityF

    static member Parse =
        parseExact authorityP
    
    static member TryParse =
        parseOption authorityP

    override x.ToString () =
        Authority.Format x

(* Path

   Taken from RFC 3986, Section 3.3 Path
   See [http://tools.ietf.org/html/rfc3986#section-3.3] *)

let private pchar =
    Set.unionMany [
        unreserved
        subDelims
        set [ ':'; '@' ] ]

let private pcharNc =
    Set.remove ':' pchar

let private segmentP =
    manySatisfy ((?>) pchar)

let private segmentNzP =
    many1Satisfy ((?>) pchar)

let private segmentNzNcP =
    many1Satisfy ((?>) pcharNc)

(* Absolute Or Empty *)

type PathAbsoluteOrEmpty =
    | PathAbsoluteOrEmpty of string list

let private pathAbsoluteOrEmptyF =
    function | PathAbsoluteOrEmpty [] -> id
             | PathAbsoluteOrEmpty xs -> slashF >> join append slashF xs

let private pathAbsoluteOrEmptyP =
    many (skipChar '/' >>. segmentP) |>> PathAbsoluteOrEmpty

type PathAbsoluteOrEmpty with

    static member Format =
        format pathAbsoluteOrEmptyF

    static member Parse =
        parseExact pathAbsoluteOrEmptyP

    static member TryParse =
        parseOption pathAbsoluteOrEmptyP

    override x.ToString () =
        PathAbsoluteOrEmpty.Format x

(* Absolute *)

type PathAbsolute =
    | PathAbsolute of string list

let private pathAbsoluteF =
    function | PathAbsolute xs -> slashF >> join append slashF xs

let private pathAbsoluteP =
    skipChar '/' >>. opt (segmentNzP .>>. many (skipChar '/' >>. segmentP))
    |>> function | Some (x, xs) -> PathAbsolute (x :: xs)
                 | _ -> PathAbsolute []

type PathAbsolute with

    static member Format =
        format pathAbsoluteF

    static member Parse =
        parseExact pathAbsoluteP

    static member TryParse =
        parseOption pathAbsoluteP

    override x.ToString () =
        PathAbsolute.Format x

(* No Scheme *)

type PathNoScheme =
    | PathNoScheme of string list

let private pathNoSchemeF =
    function | PathNoScheme xs -> join append slashF xs

let private pathNoSchemeP =
    segmentNzNcP .>>. many (slashP >>. segmentP)
    |>> fun (x, xs) -> PathNoScheme (x :: xs)

type PathNoScheme with

    static member Format =
        format pathNoSchemeF

    static member Parse =
        parseExact pathNoSchemeP

    static member TryParse =
        parseOption pathNoSchemeP

    override x.ToString () =
        PathNoScheme.Format x

(* Rootless *)

type PathRootless =
    | PathRootless of string list

let private pathRootlessF =
    function | PathRootless xs -> join append slashF xs

let private pathRootlessP =
    segmentNzP .>>. many (skipChar '/' >>. segmentP)
    |>> fun (x, xs) -> PathRootless (x :: xs)

type PathRootless with

    static member Format =
        format pathRootlessF

    static member Parse =
        parseExact pathRootlessP

    static member TryParse =
        parseOption pathRootlessP

    override x.ToString () =
        PathRootless.Format x

(* Empty *)

type PathEmpty =
    | PathEmpty

(* Query

   Taken from RFC 3986, Section 3.4 Query
   See [http://tools.ietf.org/html/rfc3986#section-3.4] *)

type Query =
    | Query of string

let internal queryF =
    function | Query x -> append "?" >> append x

let private queryChars =
    Set.unionMany [
        pchar
        set [ '/'; '?' ] ]

let internal queryP =
    skipChar '?' >>. manySatisfy ((?>) queryChars) |>> Query

type Query with

    static member Format =
        format queryF

    static member Parse =
        parseExact queryP

    static member TryParse =
        parseOption queryP

    override x.ToString () =
        Query.Format x

(* Fragment

   Taken from RFC 3986, Section 3.5 Fragment
   See [http://tools.ietf.org/html/rfc3986#section-3.5] *)

type Fragment =
    | Fragment of string

let private fragmentF =
    function | Fragment x -> append "#" >> append x

let private fragmentChars =
    Set.unionMany [
        pchar
        set [ '/'; '?' ] ]

let private fragmentP =
    skipChar '#' >>. manySatisfy ((?>) fragmentChars) |>> Fragment

type Fragment with

    static member Format =
        format fragmentF

    static member Parse =
        parseExact fragmentP

    static member TryParse =
        parseOption fragmentP

    override x.ToString () =
        Fragment.Format x

(* URI

   Taken from RFC 3986, Section 3 URI
   See [http://tools.ietf.org/html/rfc3986#section-3] *)

(* Note: In the case of absolute paths in the hierarchy, which the parser
   will correctly determine, the type system cannot adequately protect
   against an absolute path being created with two initial empty
   segments (and in the absence of a strong dependent system, it's likely
   to stay that way without extreme convolution). (Created in this case
   refers to direct instance creation).

   It is therefore possible to create an invalid URI string using the URI
   type if some care is not taken. For now this will simply have to stand
   as an allowed but "known not ideal" behaviour, under review.
   
   It is of course also possible to create invalid paths by creating strings
   which are invalid segments. Though the parser will reject these, manual
   creation will still allow this case. *)

type Uri =
    { Scheme: Scheme
      Hierarchy: HierarchyPart
      Query: Query option
      Fragment: Fragment option }

and HierarchyPart =
    | Authority of Authority * PathAbsoluteOrEmpty
    | Absolute of PathAbsolute
    | Rootless of PathRootless
    | Empty

(* Formatting *)

let private hierarchyPartF =
    function | Authority (a, p) -> append "//" >> authorityF a >> pathAbsoluteOrEmptyF p
             | Absolute p -> pathAbsoluteF p
             | Rootless p -> pathRootlessF p
             | Empty -> id

let internal uriF =
    function | { Scheme = s
                 Hierarchy = h
                 Query = q
                 Fragment = f } -> 
                    let formatters =
                        [ schemeF s
                          append ":"
                          hierarchyPartF h
                          (function | Some q -> queryF q | _ -> id) q
                          (function | Some f -> fragmentF f | _ -> id) f ]

                    fun b -> List.fold (fun b f -> f b) b formatters

(* Parsing *)

let private hierarchyPartP =
    choice [
        skipString "//" >>. authorityP .>>. pathAbsoluteOrEmptyP |>> Authority
        pathAbsoluteP |>> Absolute
        pathRootlessP |>> Rootless
        preturn Empty ]

let internal uriP =
    schemeP .>> skipChar ':' .>>. hierarchyPartP .>>. opt queryP .>>. opt fragmentP
    |>> fun (((scheme, hierarchy), query), fragment) ->
        { Scheme = scheme
          Hierarchy = hierarchy
          Query = query
          Fragment = fragment }

(* Augmentation *)

type Uri with

    static member Format =
        format uriF

    static member Parse =
        parseExact uriP

    static member TryParse =
        parseOption uriP

    override x.ToString () =
        Uri.Format x

(* Relative Reference

   Taken from RFC 3986, Section 4.2 Relative Reference
   See [http://tools.ietf.org/html/rfc3986#section-4.2] *)

type RelativeReference =
    { Relative: RelativePart
      Query: Query option
      Fragment: Fragment option }

and RelativePart =
    | Authority of Authority * PathAbsoluteOrEmpty
    | Absolute of PathAbsolute
    | NoScheme of PathNoScheme
    | Empty

let internal relativePartF =
    function | Authority (a, p) -> append "//" >> authorityF a >> pathAbsoluteOrEmptyF p
             | Absolute p -> pathAbsoluteF p
             | NoScheme p -> pathNoSchemeF p
             | Empty -> id

let internal relativeReferenceF =
    function | { Relative = r
                 Query = q
                 Fragment = f } -> 
                    let formatters =
                        [ relativePartF r
                          (function | Some q -> queryF q | _ -> id) q
                          (function | Some f -> fragmentF f | _ -> id) f ]

                    fun b -> List.fold (fun b f -> f b) b formatters

let internal relativePartP =
    choice [
        skipString "//" >>. authorityP .>>. pathAbsoluteOrEmptyP |>> Authority
        pathAbsoluteP |>> Absolute
        pathNoSchemeP |>> NoScheme
        preturn Empty ]

let internal relativeReferenceP =
    relativePartP .>>. opt queryP .>>. opt fragmentP
    |>> fun ((relative, query), fragment) ->
        { Relative = relative
          Query = query
          Fragment = fragment }

(* Augmentation *)

type RelativeReference with

    static member Format =
        format relativeReferenceF

    static member Parse =
        parseExact relativeReferenceP

    static member TryParse =
        parseOption relativeReferenceP

    override x.ToString () =
        RelativeReference.Format x

(* Absolute URI

   Taken from RFC 3986, Section 4.3 Absolute URI
   See [http://tools.ietf.org/html/rfc3986#section-4.3] *)

type AbsoluteUri =
    { Scheme: Scheme
      Hierarchy: HierarchyPart
      Query: Query option }

let internal absoluteUriF =
    function | { AbsoluteUri.Scheme = s
                 Hierarchy = h
                 Query = q } -> 
                    let formatters =
                        [ schemeF s
                          append ":"
                          hierarchyPartF h
                          (function | Some q -> queryF q | _ -> id) q ]

                    fun b -> List.fold (fun b f -> f b) b formatters

let internal absoluteUriP =
    schemeP .>> skipChar ':' .>>. hierarchyPartP .>>. opt queryP
    |>> fun ((scheme, hierarchy), query) ->
        { Scheme = scheme
          Hierarchy = hierarchy
          Query = query }

type AbsoluteUri with

    static member Format =
        format absoluteUriF

    static member Parse =
        parseExact absoluteUriP

    static member TryParse =
        parseOption absoluteUriP

    override x.ToString () =
        AbsoluteUri.Format x

(* URI Reference

   Taken from RFC 3986, Section 4.1 URI Reference
   See [http://tools.ietf.org/html/rfc3986#section-4.1] *)

type UriReference =
    | Uri of Uri
    | Relative of RelativeReference

let internal uriReferenceF =
    function | Uri x -> uriF x
             | Relative x -> relativeReferenceF x

let internal uriReferenceP =
    choice [
        attempt uriP |>> Uri
        relativeReferenceP |>> Relative ]

type UriReference with

    static member Format =
        format uriReferenceF

    static member Parse =
        parseExact uriReferenceP

    static member TryParse =
        parseOption uriReferenceP

    override x.ToString () =
        UriReference.Format x