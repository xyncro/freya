[<AutoOpen>]
module Dyfrig.Http.Negotiation

open System
open System.Globalization

(* Content Negotiation

    Taken from RFC 7231, Section 5.3
    [http://tools.ietf.org/html/rfc7231#section-5.3] *)

type private Negotiation<'a,'b> =
    { Predicate: 'a -> 'b -> bool
      Score: 'a -> 'b -> int
      Weigh: ('a * 'b) -> float }

let private negotiate (n: Negotiation<'a,'b>) r =
       List.map (fun a -> a, (List.filter (n.Predicate a) >> List.tryMaxBy (n.Score a)) r)
    >> List.choose (function | (a, Some b) -> Some (a, b) | _ -> None)
    >> List.filter (fun x -> n.Weigh x <> 0.)
    >> List.sortBy (fun x -> n.Weigh x)
    >> List.rev
    >> List.map fst

let private (==) s1 s2 =
    String.Equals (s1, s2, StringComparison.OrdinalIgnoreCase)

(* Accept

    Taken from RFC 7231, Section 5.3.2. Accept
    [http://tools.ietf.org/html/rfc7231#section-5.3.2] *)

module Accept =

    let private (|Closed|_|) =
        function | { MediaRange = MediaRange.Closed (Type x, SubType y, _) } -> Some (x, y)
                 | _ -> None

    let private (|Partial|_|) =
        function | { MediaRange = MediaRange.Partial (Type x, _) } -> Some x
                 | _ -> None

    let private (|Open|_|) =
        function | { MediaRange = MediaRange.Open _ } -> Some ()
                 | _ -> None

    let private predicate (MediaType (Type t, SubType s, _)) =
        function | Closed (t', s') when t == t' && s == s' -> true
                 | Partial t' when t == t' -> true
                 | Open _ -> true
                 | _ -> false

    let private score (MediaType (Type t, SubType s, _)) =
        function | Closed (t', s') when t == t' && s == s' -> 3
                 | Partial t' when t == t' -> 2
                 | Open _ -> 1
                 | _ -> 0

    let private weigh =
        function | (_, { AcceptableMedia.Parameters = Some { Weight = weight } }) -> weight 
                 | _ -> 1.

    let negotiate (Accept requested) =
        negotiate { Predicate = predicate
                    Score = score
                    Weigh = weigh } requested

(* Accept-Charset

    Taken from RFC 7231, Section 5.3.3. Accept-Charset
    [http://tools.ietf.org/html/rfc7231#section-5.3.3] *)

module AcceptCharset =

    let private (|CharsetC|_|) =
        function | { Charset = CharsetSpec.Charset (Charset s) } -> Some s
                 | _ -> None

    let private (|AnyC|_|) =
        function | { Charset = CharsetSpec.Any } -> Some ()
                 | _ -> None

    let private predicate (Charset s) =
        function | CharsetC s' when s == s' -> true
                 | AnyC _ -> true
                 | _ -> false

    let private score (Charset s) =
        function | CharsetC s' when s == s' -> 2
                 | AnyC _ -> 1
                 | _ -> 0

    let private weigh =
        function | (_, { AcceptableCharset.Weight = Some weight}) -> weight 
                 | _ -> 1.

    let negotiate (AcceptCharset requested) =
        negotiate { Predicate = predicate
                    Score = score
                    Weigh = weigh } requested

(* Accept-Encoding

    Taken from RFC 7231, Section 5.3.4. Accept-Encoding
    [http://tools.ietf.org/html/rfc7231#section-5.3.4] *)

module AcceptEncoding =

    let private (|NamedE|_|) =
        function | { Encoding = EncodingSpec.Encoding (Encoding e) } -> Some e
                 | _ -> None

    let private (|IdentityE|_|) =
        function | { Encoding = EncodingSpec.Identity } -> Some ()
                 | _ -> None

    let private (|AnyE|_|) =
        function | { Encoding = EncodingSpec.Any } -> Some ()
                 | _ -> None

    let private predicate (Encoding e) =
        function | NamedE e' when e == e' -> true
                 | IdentityE _ -> true
                 | AnyE _ -> true
                 | _ -> false

    let private score (Encoding e) =
        function | NamedE e' when e == e' -> 3
                 | IdentityE _ -> 2
                 | AnyE _ -> 1
                 | _ -> 0

    let private weigh =
        function | (_, { AcceptableEncoding.Weight = Some weight}) -> weight 
                 | _ -> 1.

    let negotiate (AcceptEncoding requested) =
        negotiate { Predicate = predicate
                    Score = score
                    Weigh = weigh } requested

(* Accept-Language

    Taken from RFC 7231, Section 5.3.5. Accept-Language
    [http://tools.ietf.org/html/rfc7231#section-5.3.5] *)

module AcceptLanguage =

    let private predicate (c: CultureInfo) =
        function | { Language = c' } when c = c' || c.Parent = c' -> true
                 | _ -> false

    let private score (c: CultureInfo) =
        function | { Language = c' } when c = c' -> 2
                 | { Language = c' } when c.Parent = c' -> 1
                 | _ -> 0

    let private weigh =
        function | (_, { AcceptableLanguage.Weight = Some weight}) -> weight 
                 | _ -> 1.

    let negotiate (AcceptLanguage requested) =
        negotiate { Predicate = predicate
                    Score = score
                    Weigh = weigh } requested