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
//
//----------------------------------------------------------------------------

module Freya.Types.Uri

open System
open System.ComponentModel
open System.Net
open System.Net.Sockets
open System.Text
open FParsec

(* RFC 3986

   Types, parsers and formatters implemented to mirror the specification of 
   URI semantics as defined in RFC 3986.

   Taken from [http://tools.ietf.org/html/rfc3986] *)

[<RequireQualifiedAccess>]
module Grammar =

    (* Characters

       Taken from RFC 3986, Section 2 Characters
       See [http://tools.ietf.org/html/rfc3986#section-2] *)

    let unreserved =
        Set.unionMany [
            Grammar.alpha
            Grammar.digit
            set [ '-'; '.'; '_'; '~' ] ]

    let genDelims =
        set [ ':'; '/'; '?'; '#'; '['; ']'; '@' ]

    let subDelims =
        set [ '!'; '$'; '&'; '\''; '('; ')'; '*'
              '+'; ','; ';'; '=' ]

    let reserved =
        Set.unionMany [
            genDelims
            subDelims ]

(* Percent-Encoding

   Code for percent-encoding data given some simple assumptions about what
   should be allowed through unencoded. *)

[<RequireQualifiedAccess>]
module PercentEncoding =

    (* Grammar *)

    let private pct =
        byte 0x25

    let private hexdig =
        Grammar.hexdig
        |> Set.map byte

    (* UTF-8

       Shorthand for UTF-8 encoding and decoding of strings (given
       the assumption that the .NET UTF-16/Unicode string is our
       basic string type). *)

    let private toBytes : string -> byte list =
        Encoding.UTF8.GetBytes >> List.ofArray

    let private toString : byte list -> string =
        List.toArray >> Encoding.UTF8.GetString

    (* Indices

       Simple lookups/indices for converting between bytes and the hex
       encoding of those bytes. *)

    let private hex =
        [ 0x00 .. 0xff ]
        |> List.map byte
        |> List.map (fun i -> i, toBytes (i.ToString "X2"))

    let internal byteIndex =
        hex
        |> Map.ofList

    let internal hexIndex =
        hex
        |> List.map (fun (a, b) -> (b, a))
        |> Map.ofList

    (* Parsing

       Parsing functions, providing a function to create a parser
       given a whitelist of allowed characters within the input (pct-encoded
       values are implicitly allowed, and converted to their Unicode/UTF-16
       form). *)

    let private hexdigP =
        satisfy ((?>) Grammar.hexdig)

    let private pctP =
        skipChar '%' >>. hexdigP .>>. hexdigP
        |>> fun (a, b) ->
            char (Map.find [ byte a; byte b ] hexIndex)

    let makeParser res =
        many (attempt pctP <|> satisfy ((?>) res))
        |>> fun x -> 
            new String (List.toArray x)

    (* Formatting

       Formatting functions, providing a function to create an formatter
       given a whitelist set of allowed characters within the encoded
       output. *)

    let private format res =
        let rec format r =
            function | [] -> r
                     | h :: x :: y :: t when h = pct && ((?>) hexdig x) && ((?>) hexdig y) -> format (r @ [ h; x; y ]) t
                     | h :: t when Set.contains h res -> format (r @ [ h ]) t
                     | h :: t -> format (r @ [ pct ] @ Map.find h byteIndex) t

        format []

    let makeFormatter res =
        let res = Set.map byte res

        toBytes >> format res >> toString >> append

(* Scheme

   Taken from RFC 3986, Section 3.1 Scheme
   See [http://tools.ietf.org/html/rfc3986#section-3.1] *)

(* Section 3.1 *)

type Scheme =
    | Scheme of string

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let schemeChars =
            Set.unionMany [
                Grammar.alpha
                Grammar.digit
                set [ '+'; '-'; '.' ] ]

        let schemeP =
            satisfy ((?>) Grammar.alpha) .>>. manySatisfy ((?>) schemeChars)
            |>> ((fun (x, xs) -> sprintf "%c%s" x xs) >> Scheme)

        let schemeF =
            function | Scheme x -> append x

        { Parse = schemeP
          Format = schemeF }

    static member Format =
        Formatting.format Scheme.Mapping.Format

    static member Parse =
        Parsing.parse Scheme.Mapping.Parse

    static member TryParse =
        Parsing.tryParse Scheme.Mapping.Parse

    override x.ToString () =
        Scheme.Format x

(* Authority

   Taken from RFC 3986, Section 3.2 Authority
   See [http://tools.ietf.org/html/rfc3986#section-3.2] *)

(* Section 3.2 *)

type Authority =
    | Authority of Host * Port option * UserInfo option

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let authorityP =
                 opt (attempt (UserInfo.Mapping.Parse .>> skipChar '@')) 
            .>>. Host.Mapping.Parse 
            .>>. opt Port.Mapping.Parse
             |>> fun ((user, host), port) -> Authority (host, port, user)

        let authorityF =
            function | Authority (h, p, u) ->
                        let formatters =
                            [ (function | Some u -> UserInfo.Mapping.Format u >> append "@"
                                        | _ -> id) u
                              Host.Mapping.Format h
                              (function | Some p -> Port.Mapping.Format p 
                                        | _ -> id) p ]

                        fun b -> List.fold (|>) b formatters

        { Parse = authorityP
          Format = authorityF }

    static member Format =
        Formatting.format Authority.Mapping.Format

    static member Parse =
        Parsing.parse Authority.Mapping.Parse
    
    static member TryParse =
        Parsing.tryParse Authority.Mapping.Parse

    override x.ToString () =
        Authority.Format x

(* Section 3.2.1 *)

and UserInfo =
    | UserInfo of string

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let userInfoChars =
            Set.unionMany [
                Grammar.unreserved
                Grammar.subDelims
                set [ ':' ] ]

        let parser =
            PercentEncoding.makeParser userInfoChars

        let formatter =
            PercentEncoding.makeFormatter userInfoChars

        let userInfoP =
            notEmpty parser |>> UserInfo

        let userInfoF =
            function | UserInfo x -> formatter x

        { Parse = userInfoP
          Format = userInfoF }

(* Section 3.2.2 *)

(* Note: In this instance we use the built in IP Address type and parser
   as the standard is implemented fully and seems to handle all suitable cases.

   We also make a slight restriction for practicality at the moment on
   implementing IP-literal as an IPv6 specific type, discarding IPvFuture. As
   it stands, that's unlikely to be an issue, but could perhaps be revisited. *)

and Host =
    | IPv4 of IPAddress
    | IPv6 of IPAddress
    | Name of RegName

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let ipv6Chars =
            Set.unionMany [
                Grammar.hexdig
                set [ ':' ] ]

        let ipv6AddressP =
            skipChar '[' >>. (many1Satisfy ((?>) ipv6Chars) >>= (fun x ->
                match IPAddress.TryParse x with
                | true, x when x.AddressFamily = AddressFamily.InterNetworkV6 -> preturn (IPv6 x)
                | _ -> pzero)) .>> skipChar ']'

        let ipv4Chars =
            Set.unionMany [
                Grammar.digit
                set [ '.' ] ]

        let ipv4AddressP =
            many1Satisfy ((?>) ipv4Chars) >>= (fun x ->
                match IPAddress.TryParse x with
                | true, x when x.AddressFamily = AddressFamily.InterNetwork -> preturn (IPv4 x)
                | _ -> pzero)

        let hostP =
            choice [
                attempt ipv6AddressP
                attempt ipv4AddressP
                RegName.Mapping.Parse |>> Name ]

        let hostF =
            function | IPv4 x -> append (string x)
                     | IPv6 x -> append "[" >> append (string x) >> append "]"
                     | Name x -> RegName.Mapping.Format x

        { Parse = hostP
          Format = hostF }

and RegName =
    | RegName of string

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let regNameChars =
            Set.unionMany [
                Grammar.unreserved
                Grammar.subDelims ]

        let parser =
            PercentEncoding.makeParser regNameChars

        let formatter =
            PercentEncoding.makeFormatter regNameChars

        let regNameP =
            notEmpty parser |>> RegName

        let regNameF =
            function | RegName x -> formatter x

        { Parse = regNameP
          Format = regNameF }

and Port =
    | Port of int

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let portP =
                skipChar ':' >>. puint32 
            |>> (int >> Port)

        let portF =
            function | Port x -> append ":" >> append (string x)

        { Parse = portP
          Format = portF }

(* Path

   Taken from RFC 3986, Section 3.3 Path
   See [http://tools.ietf.org/html/rfc3986#section-3.3] *)

let private pchar =
    Set.unionMany [
        Grammar.unreserved
        Grammar.subDelims
        set [ ':'; '@' ] ]

let private pcharNc =
    Set.remove ':' pchar

let private pcharParser =
    PercentEncoding.makeParser pchar

let private pcharNcParser =
    PercentEncoding.makeParser pcharNc

let private pcharFormatter =
    PercentEncoding.makeFormatter pchar

(* Absolute Or Empty *)

type PathAbsoluteOrEmpty =
    | PathAbsoluteOrEmpty of string list

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let pathAbsoluteOrEmptyP =
                many (skipChar '/' >>. pcharParser) 
            |>> PathAbsoluteOrEmpty

        let pathAbsoluteOrEmptyF =
            function | PathAbsoluteOrEmpty [] -> id
                     | PathAbsoluteOrEmpty xs -> slashF >> join pcharFormatter slashF xs

        { Parse = pathAbsoluteOrEmptyP
          Format = pathAbsoluteOrEmptyF }

    static member Format =
        Formatting.format PathAbsoluteOrEmpty.Mapping.Format

    static member Parse =
        Parsing.parse PathAbsoluteOrEmpty.Mapping.Parse

    static member TryParse =
        Parsing.tryParse PathAbsoluteOrEmpty.Mapping.Parse

    override x.ToString () =
        PathAbsoluteOrEmpty.Format x

(* Absolute *)

type PathAbsolute =
    | PathAbsolute of string list

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let pathAbsoluteP =
            skipChar '/' >>. opt (notEmpty pcharParser .>>. many (skipChar '/' >>. pcharParser))
            |>> function | Some (x, xs) -> PathAbsolute (x :: xs)
                         | _ -> PathAbsolute []

        let pathAbsoluteF =
            function | PathAbsolute xs -> slashF >> join pcharFormatter slashF xs

        { Parse = pathAbsoluteP
          Format = pathAbsoluteF }

    static member Format =
        Formatting.format PathAbsolute.Mapping.Format

    static member Parse =
        Parsing.parse PathAbsolute.Mapping.Parse

    static member TryParse =
        Parsing.tryParse PathAbsolute.Mapping.Parse

    override x.ToString () =
        PathAbsolute.Format x

(* No Scheme *)

type PathNoScheme =
    | PathNoScheme of string list

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let pathNoSchemeP =
                 notEmpty pcharNcParser 
            .>>. many (slashP >>. pcharParser)
             |>> fun (x, xs) -> PathNoScheme (x :: xs)

        let pathNoSchemeF =
            function | PathNoScheme xs -> join pcharFormatter slashF xs

        { Parse = pathNoSchemeP
          Format = pathNoSchemeF }

    static member Format =
        Formatting.format PathNoScheme.Mapping.Format

    static member Parse =
        Parsing.parse PathNoScheme.Mapping.Parse

    static member TryParse =
        Parsing.tryParse PathNoScheme.Mapping.Parse

    override x.ToString () =
        PathNoScheme.Format x

(* Rootless *)

type PathRootless =
    | PathRootless of string list

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let pathRootlessP =
                 notEmpty pcharParser 
            .>>. many (skipChar '/' >>. pcharParser)
             |>> fun (x, xs) -> PathRootless (x :: xs)

        let pathRootlessF =
            function | PathRootless xs -> join pcharFormatter slashF xs

        { Parse = pathRootlessP
          Format = pathRootlessF }

    static member Format =
        Formatting.format PathRootless.Mapping.Format

    static member Parse =
        Parsing.parse PathRootless.Mapping.Parse

    static member TryParse =
        Parsing.tryParse PathRootless.Mapping.Parse

    override x.ToString () =
        PathRootless.Format x

(* Query

   Taken from RFC 3986, Section 3.4 Query
   See [http://tools.ietf.org/html/rfc3986#section-3.4] *)

type Query =
    | Query of string

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let queryChars =
            Set.unionMany [
                pchar
                set [ '/'; '?' ] ]

        let parser =
            PercentEncoding.makeParser queryChars

        let formatter =
            PercentEncoding.makeFormatter queryChars

        let queryP =
            skipChar '?' >>. parser |>> Query

        let queryF =
            function | Query x -> append "?" >> formatter x

        { Parse = queryP
          Format = queryF }

    static member Format =
        Formatting.format Query.Mapping.Format

    static member Parse =
        Parsing.parse Query.Mapping.Parse

    static member TryParse =
        Parsing.tryParse Query.Mapping.Parse

    override x.ToString () =
        Query.Format x

(* Fragment

   Taken from RFC 3986, Section 3.5 Fragment
   See [http://tools.ietf.org/html/rfc3986#section-3.5] *)

type Fragment =
    | Fragment of string

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =
    
        let fragmentChars =
            Set.unionMany [
                pchar
                set [ '/'; '?' ] ]

        let parser =
            PercentEncoding.makeParser fragmentChars

        let formatter =
            PercentEncoding.makeFormatter fragmentChars

        let fragmentP =
            skipChar '#' >>. parser |>> Fragment

        let fragmentF =
            function | Fragment x -> append "#" >> formatter x

        { Parse = fragmentP
          Format = fragmentF }

    static member Format =
        Formatting.format Fragment.Mapping.Format

    static member Parse =
        Parsing.parse Fragment.Mapping.Parse

    static member TryParse =
        Parsing.tryParse Fragment.Mapping.Parse

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
    | Uri of Scheme * HierarchyPart * Query option * Fragment option

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let uriP =
                 Scheme.Mapping.Parse .>> skipChar ':'
            .>>. HierarchyPart.Mapping.Parse 
            .>>. opt Query.Mapping.Parse
            .>>. opt Fragment.Mapping.Parse
             |>> fun (((scheme, hierarchy), query), fragment) ->
                Uri (scheme, hierarchy, query, fragment)

        let uriF =
            function | Uri (s, h, q, f) -> 
                        let formatters =
                            [ Scheme.Mapping.Format s
                              append ":"
                              HierarchyPart.Mapping.Format h
                              (function | Some q -> Query.Mapping.Format q 
                                        | _ -> id) q
                              (function | Some f -> Fragment.Mapping.Format f 
                                        | _ -> id) f ]

                        fun b -> List.fold (|>) b formatters

        { Parse = uriP
          Format = uriF }

    static member Format =
        Formatting.format Uri.Mapping.Format

    static member Parse =
        Parsing.parse Uri.Mapping.Parse

    static member TryParse =
        Parsing.tryParse Uri.Mapping.Parse

    override x.ToString () =
        Uri.Format x

and HierarchyPart =
    | Authority of Authority * PathAbsoluteOrEmpty
    | Absolute of PathAbsolute
    | Rootless of PathRootless
    | Empty

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let authorityP =
                 skipString "//" >>. Authority.Mapping.Parse 
            .>>. PathAbsoluteOrEmpty.Mapping.Parse 
             |>> Authority

        let hierarchyPartP =
            choice [
                authorityP
                PathAbsolute.Mapping.Parse |>> Absolute
                PathRootless.Mapping.Parse |>> Rootless
                preturn Empty ]

        let authorityF (a, p)=
                append "//" 
             >> Authority.Mapping.Format a 
             >> PathAbsoluteOrEmpty.Mapping.Format p

        let hierarchyPartF =
            function | Authority (a, p) -> authorityF (a, p)
                     | Absolute p -> PathAbsolute.Mapping.Format p
                     | Rootless p -> PathRootless.Mapping.Format p
                     | Empty -> id

        { Parse = hierarchyPartP
          Format = hierarchyPartF }

(* Relative Reference

   Taken from RFC 3986, Section 4.2 Relative Reference
   See [http://tools.ietf.org/html/rfc3986#section-4.2] *)

type RelativeReference =
    | RelativeReference of RelativePart * Query option * Fragment option

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let relativeReferenceP =
                 RelativePart.Mapping.Parse
            .>>. opt Query.Mapping.Parse
            .>>. opt Fragment.Mapping.Parse
             |>> fun ((relative, query), fragment) ->
                RelativeReference (relative, query, fragment)

        let relativeReferenceF =
            function | RelativeReference (r, q, f) -> 
                        let formatters =
                            [ RelativePart.Mapping.Format r
                              (function | Some q -> Query.Mapping.Format q
                                        | _ -> id) q
                              (function | Some f -> Fragment.Mapping.Format f
                                        | _ -> id) f ]

                        fun b -> List.fold (|>) b formatters

        { Parse = relativeReferenceP
          Format = relativeReferenceF }

    static member Format =
        Formatting.format RelativeReference.Mapping.Format

    static member Parse =
        Parsing.parse RelativeReference.Mapping.Parse

    static member TryParse =
        Parsing.tryParse RelativeReference.Mapping.Parse

    override x.ToString () =
        RelativeReference.Format x

and RelativePart =
    | Authority of Authority * PathAbsoluteOrEmpty
    | Absolute of PathAbsolute
    | NoScheme of PathNoScheme
    | Empty

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let authorityP =
                 skipString "//" >>. Authority.Mapping.Parse
            .>>. PathAbsoluteOrEmpty.Mapping.Parse 
             |>> Authority

        let relativePartP =
            choice [
                authorityP
                PathAbsolute.Mapping.Parse |>> Absolute
                PathNoScheme.Mapping.Parse |>> NoScheme
                preturn Empty ]

        let authorityF (a, p) =
                append "//" 
             >> Authority.Mapping.Format a 
             >> PathAbsoluteOrEmpty.Mapping.Format p

        let relativePartF =
            function | Authority (a, p) -> authorityF (a, p)
                     | Absolute p -> PathAbsolute.Mapping.Format p
                     | NoScheme p -> PathNoScheme.Mapping.Format p
                     | Empty -> id

        { Parse = relativePartP
          Format = relativePartF }

(* Absolute URI

   Taken from RFC 3986, Section 4.3 Absolute URI
   See [http://tools.ietf.org/html/rfc3986#section-4.3] *)

type AbsoluteUri =
    | AbsoluteUri of Scheme * HierarchyPart * Query option

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let absoluteUriP =
                 Scheme.Mapping.Parse .>> skipChar ':' 
            .>>. HierarchyPart.Mapping.Parse 
            .>>. opt Query.Mapping.Parse
             |>> fun ((scheme, hierarchy), query) ->
                AbsoluteUri (scheme, hierarchy, query)

        let absoluteUriF =
            function | AbsoluteUri (s, h, q) -> 
                        let formatters =
                            [ Scheme.Mapping.Format s
                              append ":"
                              HierarchyPart.Mapping.Format h
                              (function | Some q -> Query.Mapping.Format q
                                        | _ -> id) q ]

                        fun b -> List.fold (|>) b formatters

        { Parse = absoluteUriP
          Format = absoluteUriF }

    static member Format =
        Formatting.format AbsoluteUri.Mapping.Format

    static member Parse =
        Parsing.parse AbsoluteUri.Mapping.Parse

    static member TryParse =
        Parsing.tryParse AbsoluteUri.Mapping.Parse

    override x.ToString () =
        AbsoluteUri.Format x

(* URI Reference

   Taken from RFC 3986, Section 4.1 URI Reference
   See [http://tools.ietf.org/html/rfc3986#section-4.1] *)

type UriReference =
    | Uri of Uri
    | Relative of RelativeReference

    [<EditorBrowsable (EditorBrowsableState.Never)>]
    static member Mapping =

        let uriReferenceP =
            choice [
                attempt Uri.Mapping.Parse |>> Uri
                RelativeReference.Mapping.Parse |>> Relative ]

        let uriReferenceF =
            function | Uri x -> Uri.Mapping.Format x
                     | Relative x -> RelativeReference.Mapping.Format x

        { Parse = uriReferenceP
          Format = uriReferenceF }

    static member Format =
        Formatting.format UriReference.Mapping.Format

    static member Parse =
        Parsing.parse UriReference.Mapping.Parse

    static member TryParse =
        Parsing.tryParse UriReference.Mapping.Parse

    override x.ToString () =
        UriReference.Format x