[<AutoOpen>]
module Freya.Machine.Negotiation

open System
open Freya.Typed

(* Content Negotiation

   Taken from RFC 7231, Section 5.3
   [http://tools.ietf.org/html/rfc7231#section-5.3] *)

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

(* Accept *)

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

(* Accept-Charset *)

module AcceptCharset =

    let private predicate (Charset s) =
        function | { AcceptableCharset.Charset = CharsetRange.Charset (Charset s') } when s == s' -> true
                 | { Charset = CharsetRange.Any } -> true
                 | _ -> false

    let private score (Charset s) =
        function | { AcceptableCharset.Charset = CharsetRange.Charset (Charset s') } when s == s' -> 2
                 | { Charset = CharsetRange.Any } -> 1
                 | _ -> 0

    let private weigh =
        function | { AcceptableCharset.Weight = Some weight } -> weight 
                 | _ -> 1.

    let negotiate (AcceptCharset requested) =
        negotiate { Predicate = predicate
                    Score = score
                    Weigh = weigh } requested

(* Accept-Encoding *)

module AcceptEncoding =

    let private predicate (ContentCoding e) =
        function | { AcceptableEncoding.Encoding = Coding (ContentCoding e') } when e == e' -> true
                 | { Encoding = Identity } -> true
                 | { Encoding = EncodingRange.Any } -> true
                 | _ -> false

    let private score (ContentCoding e) =
        function | { AcceptableEncoding.Encoding = Coding (ContentCoding e') } when e == e' -> 3
                 | { Encoding = Identity } -> 2
                 | { Encoding = EncodingRange.Any } -> 1
                 | _ -> 0

    let private weigh =
        function | { AcceptableEncoding.Weight = Some weight } -> weight 
                 | _ -> 1.

    let negotiate (AcceptEncoding requested) =
        negotiate { Predicate = predicate
                    Score = score
                    Weigh = weigh } requested

(* Accept-Language *)

module AcceptLanguage =

    (* Note: This is intended to approximate (hopefully closely) the semantics
       of Basic Filtering as specified in Section 3.3.1 of RFC 4647.

       See [http://tools.ietf.org/html/rfc4647#section-3.3.1] *)

    let private listOfTag (tag: LanguageTag) =
        let language, extensions =
            (function | Language (language, Some extensions) -> [ language ], extensions
                      | Language (language, _) -> [ language ], []) tag.Language

        let script =
            (function | Some (Script script) -> [ script ]
                      | _ -> []) tag.Script

        let region =
            (function | Some (Region region) -> [ region ]
                      | _ -> []) tag.Region

        let variant =
            (function | Variant variant -> variant) tag.Variant

        List.concat [
            language
            extensions
            script
            region
            variant ]

    let private predicate (t: LanguageTag) =
        function | { AcceptableLanguage.Language = Range range } -> 
                        Seq.zip range (listOfTag t) |> Seq.forall (fun (a, b) -> a == b)
                 | _ -> true

    // TODO: Re-evaluate this scoring, potentially looking for max (tag length, range length).

    let private score (t: LanguageTag) =
        function | { AcceptableLanguage.Language = Range range } -> range.Length
                 | _ -> 0

    let private weigh =
        function | { AcceptableLanguage.Weight = Some weight } -> weight 
                 | _ -> 1.

    let negotiate (AcceptLanguage requested) =
        negotiate { Predicate = predicate
                    Score = score
                    Weigh = weigh } requested