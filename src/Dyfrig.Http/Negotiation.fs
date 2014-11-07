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
    function | MediaRangeSpec.Closed (Type x, SubType y) -> Some (x, y)
             | _ -> None

let private (|PartialM|_|) =
    function | MediaRangeSpec.Partial (Type x) -> Some x
             | _ -> None

let private (|OpenM|_|) =
    function | MediaRangeSpec.Open -> Some ()
             | _ -> None

let private matchAccept (MediaType (Type t, SubType s)) =
    function | ClosedM (t', s') when t == t' && s == s' -> true
             | PartialM t' when t == t' -> true
             | OpenM _ -> true
             | _ -> false

let private scoreAccept (MediaType (Type t, SubType s)) =
    function | ClosedM (t', s') when t == t' && s == s' -> 3
             | PartialM t' when t == t' -> 2
             | OpenM _ -> 1
             | _ -> 0

let private selectAccept (available: MediaType list) =
    List.map (fun r -> 
        available 
        |> List.filter (fun a -> matchAccept a.MediaType r.MediaRange.MediaRange)
        |> List.sortBy (fun a -> scoreAccept a.MediaType r.MediaRange.MediaRange)) >> List.concat

let negotiateAccept (available: MediaType list) (Accept requested) =
    prepare requested 
    |> selectAccept available

(* Accept-Charset

    Taken from RFC 7231, Section 5.3.3. Accept-Charset
    [http://tools.ietf.org/html/rfc7231#section-5.3.3] *)

let private (|CharsetC|_|) =
    function | CharsetSpec.Charset (Charset s) -> Some s
             | _ -> None

let private (|AnyC|_|) =
    function | CharsetSpec.Any -> Some ()
             | _ -> None

let private matchAcceptCharset =
    function | Charset s, CharsetC s' when s == s' -> true
             | _, AnyC _ -> true
             | _ -> false

let private selectAcceptCharset (available: Charset list) =
    List.map (fun r ->
        available
        |> List.filter (fun a -> matchAcceptCharset (a, r.Charset))) >> List.concat

let negotiateAcceptCharset (available: Charset list) (AcceptCharset requested) =
    prepare requested 
    |> selectAcceptCharset available

(* Accept-Encoding

    Taken from RFC 7231, Section 5.3.4. Accept-Encoding
    [http://tools.ietf.org/html/rfc7231#section-5.3.4] *)

let private (|NamedE|_|) =
    function | EncodingSpec.Encoding (Encoding e) -> Some e
             | _ -> None

let private (|IdentityE|_|) =
    function | EncodingSpec.Identity -> Some ()
             | _ -> None

let private (|AnyE|_|) =
    function | EncodingSpec.Any -> Some ()
             | _ -> None

let private matchAcceptEncoding =
    function | Encoding e, NamedE e' when e == e' -> true
             | _, IdentityE _ -> true
             | _, AnyE _ -> true
             | _ -> false

let private selectAcceptEncoding (available: Encoding list) =
    List.map (fun r ->
        available
        |> List.filter (fun a -> matchAcceptEncoding (a, r.Encoding))) >> List.concat

let negotiateAcceptEncoding (available: Encoding list) (AcceptEncoding requested) =
    prepare requested 
    |> selectAcceptEncoding available

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

let negotiateAcceptLanguage (available: CultureInfo list) (AcceptLanguage requested) =
    prepare requested 
    |> selectAcceptLanguage available

