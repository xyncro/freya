[<AutoOpen>]
module Freya.Machine.Negotiation

open Freya.Types.Http
open Freya.Types.Language

(* Content Negotiation

   Taken from RFC 7231, Section 5.3
   [http://tools.ietf.org/html/rfc7231#section-5.3] *)

(* Accept *)

module Accept =

    let private max (MediaType (Type t, SubType s, _)) =
        function | { MediaRange = Closed (Type t', SubType s', _) } when t == t' && s == s' -> Some 0
                 | { MediaRange = MediaRange.Partial (Type t', _) } when t == t' -> Some 1
                 | { MediaRange = Open _ } -> Some 2
                 | _ -> None

    let private map requested =
        List.map (fun (x: MediaType) ->
            x, List.chooseMaxBy (max x) requested)

    let private sort =
        List.sortBy (fun (x, y) ->
            (function | Some { Parameters = Some { Weight = weight } } -> 1. - weight
                      | _ -> 0.) y)

    let private choose =
        List.choose (fun (x, y) ->
            (function | Some { Parameters = Some { Weight = weight } } when weight > 0. -> Some x
                      | Some { Parameters = None } -> Some x
                      | _ -> None) y)

    let private run requested =
           map requested 
        >> sort
        >> choose

    let negotiate supported =
        function | Some (Accept requested) -> Some (run requested supported)
                 | _ -> None

(* Accept-Charset *)

module AcceptCharset =

    let private max (Charset s) =
        function | { AcceptableCharset.Charset = CharsetRange.Charset (Charset s') } when s == s' -> Some 0
                 | { Charset = CharsetRange.Any } -> Some 1
                 | _ -> None

    let private map requested =
        List.map (fun (x: Charset) ->
            x, List.chooseMaxBy (max x) requested)

    let private sort =
        List.sortBy (fun (x, y) ->
            (function | Some { AcceptableCharset.Weight = Some weight } -> 1. - weight
                      | _ -> 0.) y)

    let private choose =
        List.choose (fun (x, y) ->
            (function | Some { AcceptableCharset.Weight = Some weight } when weight > 0. -> Some x
                      | Some { AcceptableCharset.Weight = None } -> Some x
                      | _ -> None) y)

    let private run requested =
           map requested 
        >> sort
        >> choose

    let negotiate supported =
        function | Some (AcceptCharset requested) -> Some (run requested supported)
                 | _ -> None

(* Accept-Encoding *)

module AcceptEncoding =

    // TODO: Better Content-Coding Negotiation - proper support of identity, etc.

    let private max (ContentCoding c) =
        function | { AcceptableEncoding.Encoding = EncodingRange.Coding (ContentCoding c') } when c == c' -> Some 0
                 | { Encoding = EncodingRange.Any } -> Some 1
                 | _ -> None

    let private map requested =
        List.map (fun (x: ContentCoding) ->
            x, List.chooseMaxBy (max x) requested)

    let private sort =
        List.sortBy (fun (x, y) ->
            (function | Some { AcceptableEncoding.Weight = Some weight } -> 1. - weight
                      | _ -> 0.) y)

    let private choose =
        List.choose (fun (x, y) ->
            (function | Some { AcceptableEncoding.Weight = Some weight } when weight > 0. -> Some x
                      | Some { AcceptableEncoding.Weight = None } -> Some x
                      | _ -> None) y)

    let private run requested =
           map requested 
        >> sort
        >> choose

    let negotiate supported=
        function | Some (AcceptEncoding requested) -> Some (run requested supported)
                 | _ -> None

(* Accept-Language *)

module AcceptLanguage =

    (* Note: This is intended to approximate the semantics
       of Basic Filtering as specified in Section 3.3.1 of RFC 4647.

       See [http://tools.ietf.org/html/rfc4647#section-3.3.1] *)

    let private toList tag =
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

    let private eq tag =
        Seq.zip (toList tag) >> Seq.forall ((<||) (==))

    let private sort =
        List.sortBy (fun (x: AcceptableLanguage) -> 
            (function | Some x -> 1. - x
                      | _ -> 0. ) x.Weight)

    let private filter =
        List.filter (fun (x: AcceptableLanguage) ->
            (function | Some 0. -> false
                      | _ -> true) x.Weight)

    let private map supported =
        List.map (fun (x: AcceptableLanguage) ->
            (function | Range x -> List.filter (flip eq x) supported
                      | Any -> supported) x.Language)
    
    let private run supported =
           sort
        >> filter
        >> map supported
        >> Seq.concat
        >> Seq.distinct
        >> Seq.toList

    let negotiate supported =
        function | Some (AcceptLanguage requested) -> Some (run supported requested)
                 | _ -> None