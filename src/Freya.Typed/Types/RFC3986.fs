[<AutoOpen>]
module Freya.Typed.RFC3986

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
             | PathAbsoluteOrEmpty xs -> slashF >> join slashF append xs

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
    function | PathAbsolute xs -> slashF >> join slashF append xs

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
    function | PathNoScheme xs -> join slashF append xs

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
    function | PathRootless xs -> join slashF append xs

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
      Hierarchy: Hierarchy
      Query: Query option
      Fragment: Fragment option }

and Hierarchy =
    | Authority of Authority * PathAbsoluteOrEmpty
    | Absolute of PathAbsolute
    | Rootless of PathRootless
    | Empty

let private hierPartF =
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
                          hierPartF h
                          (function | Some q -> queryF q | _ -> id) q
                          (function | Some f -> fragmentF f | _ -> id) f ]

                    fun b -> List.fold (fun b f -> f b) b formatters

let private hierPartP =
    choice [
        skipString "//" >>. authorityP .>>. pathAbsoluteOrEmptyP |>> Authority
        pathAbsoluteP |>> Absolute
        pathRootlessP |>> Rootless
        preturn Empty ]

let internal uriP =
    schemeP .>> skipChar ':' .>>. hierPartP .>>. opt queryP .>>. opt fragmentP
    |>> fun (((scheme, hierarchy), query), fragment) ->
        { Scheme = scheme
          Hierarchy = hierarchy
          Query = query
          Fragment = fragment }

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

type Relative =
    | Authority of Authority * PathAbsoluteOrEmpty
    | Absolute of PathAbsolute
    | NoScheme of PathNoScheme
    | Empty

let internal relativeF =
    function | Authority (a, p) -> append "//" >> authorityF a >> pathAbsoluteOrEmptyF p
             | Absolute p -> pathAbsoluteF p
             | NoScheme p -> pathNoSchemeF p
             | Empty -> id

let internal relativeP =
    choice [
        skipString "//" >>. authorityP .>>. pathAbsoluteOrEmptyP |>> Authority
        pathAbsoluteP |>> Absolute
        pathNoSchemeP |>> NoScheme
        preturn Empty ]

type Relative with

    static member Format =
        format relativeF

    static member Parse =
        parseExact relativeP

    static member TryParse =
        parseOption relativeP

    override x.ToString () =
        Relative.Format x

(* Relative URI

   Taken from RFC 3986, Section 4.3 Relative Reference
   See [http://tools.ietf.org/html/rfc3986#section-4.2] *)

(* Note: This type has been renamed RelativeUri to avoid confusion
   with Relative. *)

type RelativeUri =
    { Relative: Relative
      Query: Query option
      Fragment: Fragment option }

let internal relativeUriF =
    function | { Relative = r
                 Query = q
                 Fragment = f } -> 
                    let formatters =
                        [ relativeF r
                          (function | Some q -> queryF q | _ -> id) q
                          (function | Some f -> fragmentF f | _ -> id) f ]

                    fun b -> List.fold (fun b f -> f b) b formatters

let internal relativeUriP =
    relativeP .>>. opt queryP .>>. opt fragmentP
    |>> fun ((relative, query), fragment) ->
        { Relative = relative
          Query = query
          Fragment = fragment }

type RelativeUri with

    static member Format =
        format relativeUriF

    static member Parse =
        parseExact relativeUriP

    static member TryParse =
        parseOption relativeUriP

    override x.ToString () =
        RelativeUri.Format x

(* Absolute URI

   Taken from RFC 3986, Section 4.3 Absolute URI
   See [http://tools.ietf.org/html/rfc3986#section-4.3] *)

type AbsoluteUri =
    { Scheme: Scheme
      Hierarchy: Hierarchy
      Query: Query option }

let internal absoluteUriF =
    function | { AbsoluteUri.Scheme = s
                 Hierarchy = h
                 Query = q } -> 
                    let formatters =
                        [ schemeF s
                          append ":"
                          hierPartF h
                          (function | Some q -> queryF q | _ -> id) q ]

                    fun b -> List.fold (fun b f -> f b) b formatters

let internal absoluteUriP =
    schemeP .>> skipChar ':' .>>. hierPartP .>>. opt queryP
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
    | Relative of RelativeUri

let internal uriReferenceF =
    function | Uri x -> uriF x
             | Relative x -> relativeUriF x

let internal uriReferenceP =
    choice [
        attempt uriP |>> Uri
        relativeUriP |>> Relative ]

type UriReference with

    static member Format =
        format uriReferenceF

    static member Parse =
        parseExact uriReferenceP

    static member TryParse =
        parseOption uriReferenceP

    override x.ToString () =
        UriReference.Format x