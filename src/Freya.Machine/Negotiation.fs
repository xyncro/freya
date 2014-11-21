[<AutoOpen>]
module Freya.Machine.Negotiation

open System
open Freya.Typed

(* Content Negotiation

   Taken from RFC 7231, Section 5.3
   [http://tools.ietf.org/html/rfc7231#section-5.3] *)

// TODO: Review Negotiation Algorithms for type specific implementation standards

type private Negotiation<'a,'b> =
    { Predicate: 'a -> 'b -> bool
      Score: 'a -> 'b -> int
      Weigh: 'b -> float }

let private negotiate (n: Negotiation<'a,'b>) r =
       List.map (fun a -> a, (List.filter (n.Predicate a) >> List.tryMaxBy (n.Score a)) r)
    >> List.choose (function | (a, Some b) -> Some (a, b) | _ -> None)
    >> List.filter (fun (_, b) -> n.Weigh b <> 0.)
    >> List.sortBy (fun (_, b) -> n.Weigh b)
    >> List.rev
    >> List.map fst

let private (==) s1 s2 =
    String.Equals (s1, s2, StringComparison.OrdinalIgnoreCase)

(* Accept

   Taken from RFC 7231, Section 5.3.2. Accept
   [http://tools.ietf.org/html/rfc7231#section-5.3.2] *)

module Accept =

    let private predicate (MediaType (Type t, SubType s, _)) =
        function | { AcceptableMedia.MediaRange = Closed (Type t', SubType s', _) } when t == t' && s == s' -> true
                 | { MediaRange = MediaRange.Partial (Type t', _) } when t == t' -> true
                 | { MediaRange = Open _ } -> true
                 | _ -> false

    let private score (MediaType (Type t, SubType s, _)) =
        function | { AcceptableMedia.MediaRange = Closed (Type t', SubType s', _) } when t == t' && s == s' -> 3
                 | { MediaRange = MediaRange.Partial (Type t', _) } when t == t' -> 2
                 | { MediaRange = Open _ } -> 1
                 | _ -> 0

    let private weigh =
        function | { AcceptableMedia.Parameters = Some { Weight = weight } } -> weight 
                 | _ -> 1.

    let negotiate (Accept requested) =
        negotiate { Predicate = predicate
                    Score = score
                    Weigh = weigh } requested

(* Accept-Charset

   Taken from RFC 7231, Section 5.3.3. Accept-Charset
   [http://tools.ietf.org/html/rfc7231#section-5.3.3] *)

module AcceptCharset =

    let private predicate (Charset s) =
        function | { AcceptableCharset.Charset = CharsetSpec.Charset (Charset s') } when s == s' -> true
                 | { Charset = CharsetSpec.Any } -> true
                 | _ -> false

    let private score (Charset s) =
        function | { AcceptableCharset.Charset = CharsetSpec.Charset (Charset s') } when s == s' -> 2
                 | { Charset = CharsetSpec.Any } -> 1
                 | _ -> 0

    let private weigh =
        function | { AcceptableCharset.Weight = Some weight } -> weight 
                 | _ -> 1.

    let negotiate (AcceptCharset requested) =
        negotiate { Predicate = predicate
                    Score = score
                    Weigh = weigh } requested

(* Accept-Encoding

   Taken from RFC 7231, Section 5.3.4. Accept-Encoding
   [http://tools.ietf.org/html/rfc7231#section-5.3.4] *)

module AcceptEncoding =

    let private predicate (Encoding e) =
        function | { AcceptableEncoding.Encoding = EncodingSpec.Encoding (Encoding e') } when e == e' -> true
                 | { Encoding = EncodingSpec.Identity } -> true
                 | { Encoding = EncodingSpec.Any } -> true
                 | _ -> false

    let private score (Encoding e) =
        function | { AcceptableEncoding.Encoding = EncodingSpec.Encoding (Encoding e') } when e == e' -> 3
                 | { Encoding = EncodingSpec.Identity } -> 2
                 | { Encoding = EncodingSpec.Any } -> 1
                 | _ -> 0

    let private weigh =
        function | { AcceptableEncoding.Weight = Some weight } -> weight 
                 | _ -> 1.

    let negotiate (AcceptEncoding requested) =
        negotiate { Predicate = predicate
                    Score = score
                    Weigh = weigh } requested

(* Accept-Language

   Taken from RFC 7231, Section 5.3.5. Accept-Language
   [http://tools.ietf.org/html/rfc7231#section-5.3.5] *)

module AcceptLanguage =

// TODO: Language Tag / Language Range Matching
// See RFC 4647

//    let private predicate (c: CultureInfo) =
//        function | { AcceptableLanguage.Language = c' } when c = c' || c.Parent = c' -> true
//                 | _ -> false
//
//    let private score (c: CultureInfo) =
//        function | { AcceptableLanguage.Language = c' } when c = c' -> 2
//                 | { Language = c' } when c.Parent = c' -> 1
//                 | _ -> 0
//
//    let private weigh =
//        function | { AcceptableLanguage.Weight = Some weight } -> weight 
//                 | _ -> 1.

    let negotiate (AcceptLanguage requested) =
        id

//        negotiate { Predicate = predicate
//                    Score = score
//                    Weigh = weigh } requested