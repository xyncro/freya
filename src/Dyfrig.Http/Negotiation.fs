[<AutoOpen>]
module Dyfrig.Http.Negotiation

open System
open System.Globalization

let private (==) s1 s2 =
    String.Equals (s1, s2, StringComparison.OrdinalIgnoreCase)

let inline private weight (a: ^a) =
    ((^a) : (member Weight: float option) a)

let inline private prepare requested =
    requested
    |> List.filter (fun r -> weight r <> Some 0.)
    |> List.sortBy (fun r -> weight r |> Option.getOrElse 1.)
    |> List.rev

(* Content Negotiation
            
    Taken from RFC 7231, Section 5.3
    [http://tools.ietf.org/html/rfc7231#section-5.3] *)

(* Accept

    Taken from RFC 7231, Section 5.3.2. Accept
    [http://tools.ietf.org/html/rfc7231#section-5.3.2] *)

let private (|ClosedM|_|) =
    function | MediaRange.Specified (Closed (MediaType x, MediaSubType y)) -> Some (x, y)
             | _ -> None

let private (|PartialM|_|) =
    function | MediaRange.Partial (Partial (MediaType x)) -> Some x
             | _ -> None

let private (|OpenM|_|) =
    function | MediaRange.Partial (Open) -> Some ()
             | _ -> None

let private matchAccept (Closed (MediaType t, MediaSubType s)) =
    function | ClosedM (t', s') when t == t' && s == s' -> true
             | PartialM t' when t == t' -> true
             | OpenM _ -> true
             | _ -> false

let private scoreAccept (Closed (MediaType t, MediaSubType s)) =
    function | ClosedM (t', s') when t == t' && s == s' -> 3
             | PartialM t' when t == t' -> 2
             | OpenM _ -> 1
             | _ -> 0

let private selectAccept (available: SpecifiedMediaRange list) =
    List.map (fun r -> 
        available 
        |> List.filter (fun a -> matchAccept a r.MediaRange)
        |> List.sortBy (fun a -> scoreAccept a r.MediaRange)) >> List.concat

let negotiateAccept (available: SpecifiedMediaRange list) =
    prepare >> selectAccept available

(* Accept-Charset

    Taken from RFC 7231, Section 5.3.3. Accept-Charset
    [http://tools.ietf.org/html/rfc7231#section-5.3.3] *)

let private (|NamedC|_|) =
    function | Charset.Specified (SpecifiedCharset.Named s) -> Some s
             | _ -> None

let private (|AnyC|_|) =
    function | Charset.Any -> Some ()
             | _ -> None

let private matchAcceptCharset =
    function | SpecifiedCharset.Named s, NamedC s' when s == s' -> true
             | _, AnyC _ -> true
             | _ -> false

let private selectAcceptCharset (available: SpecifiedCharset list) =
    List.map (fun r ->
        available
        |> List.filter (fun a -> matchAcceptCharset (a, r.Charset))) >> List.concat

let negotiateAcceptCharset (available: SpecifiedCharset list) =
    prepare >> selectAcceptCharset available

(* Accept-Encoding

    Taken from RFC 7231, Section 5.3.4. Accept-Encoding
    [http://tools.ietf.org/html/rfc7231#section-5.3.4] *)

let private (|NamedE|_|) =
    function | Encoding.Specified (SpecifiedEncoding.Named e) -> Some e
             | _ -> None

let private (|IdentityE|_|) =
    function | Encoding.Specified (SpecifiedEncoding.Identity) -> Some ()
             | _ -> None

let private (|AnyE|_|) =
    function | Encoding.Any -> Some ()
             | _ -> None

let private matchAcceptEncoding =
    function | SpecifiedEncoding.Named e, NamedE e' when e == e' -> true
             | SpecifiedEncoding.Identity, IdentityE _ -> true
             | _, AnyE _ -> true
             | _ -> false

let private selectAcceptEncoding (available: SpecifiedEncoding list) =
    List.map (fun r ->
        available
        |> List.filter (fun a -> matchAcceptEncoding (a, r.Encoding))) >> List.concat

let negotiateAcceptEncoding (available: SpecifiedEncoding list) =
    prepare >> selectAcceptEncoding available

(* Accept-Language

    Taken from RFC 7231, Section 5.3.5. Accept-Language
    [http://tools.ietf.org/html/rfc7231#section-5.3.5] *)

let private matchAcceptLanguage (c: CultureInfo) =
    function | c' when c = c' || c.Parent = c' -> true
             | _ -> false

let private scoreAcceptLanguage (c: CultureInfo) =
    function | c' when c = c' -> 2
             | c' when c.Parent = c' -> 1
             | _ -> 0

let private selectAcceptLanguage (available: CultureInfo list) =
    List.map (fun r -> 
        available 
        |> List.filter (fun a -> matchAcceptLanguage a r.Language)
        |> List.sortBy (fun a -> scoreAcceptLanguage a r.Language)) >> List.concat

let negotiateAcceptLanguage (available: CultureInfo list) =
    prepare >> selectAcceptLanguage available

