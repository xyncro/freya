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

[<AutoOpen>]
module Freya.Machine.Extensions.Http.Domain

open System
open Freya.Core
open Freya.Core.Operators
open Freya.Types.Http
open Freya.Types.Language

(* Cache Control *)

[<RequireQualifiedAccess>]
module CacheControl =

    (* If-Match *)

    [<RequireQualifiedAccess>]
    module IfMatch =

        (* Decisions *)

        let requested ifMatch =
                Option.isSome 
            <!> ifMatch

        let any ifMatch =
                (=) (Some (IfMatch IfMatchChoice.Any)) 
            <!> ifMatch

    (* If-Modified-Since *)

    [<RequireQualifiedAccess>]
    module IfModifiedSince =

        (* Decisions *)

        let requested ifModifiedSince =
                Option.isSome 
            <!> ifModifiedSince

        let valid ifModifiedSince =
                function | Some (IfModifiedSince x) -> x < DateTime.UtcNow
                         | _ -> false
            <!> ifModifiedSince

        let modified ifModifiedSince lastModified =
                (fun x y -> (function | Some lm, Some (IfModifiedSince ms) -> lm > ms
                                      | _ -> false) (x, y))
            <!> lastModified
            <*> ifModifiedSince

    (* If-None-Match *)

    [<RequireQualifiedAccess>]
    module IfNoneMatch =

        (* Decisions *)

        let requested ifNoneMatch =
                Option.isSome 
            <!> ifNoneMatch

        let any ifNoneMatch =
                (=) (Some (IfNoneMatch IfNoneMatchChoice.Any)) 
            <!> ifNoneMatch

    (* If-Unmodified-Since *)

    [<RequireQualifiedAccess>]
    module IfUnmodifiedSince =

        (* Decisions *)

        let requested ifUnmodifiedSince =
                Option.isSome 
            <!> ifUnmodifiedSince

        let valid ifUnmodifiedSince =
                function | Some (IfUnmodifiedSince x) -> x < DateTime.UtcNow
                         | _ -> false
            <!> ifUnmodifiedSince

        let unmodified ifUnmodifiedSince lastModified =
                (fun x y -> (function | Some lm, Some (IfUnmodifiedSince us) -> lm < us
                                      | _ -> true) (x, y))
            <!> lastModified
            <*> ifUnmodifiedSince

(* Content Negotiation *)

[<RequireQualifiedAccess>]
module ContentNegotiation =

    (* Operators *)

    let (==) s1 s2 =
        String.Equals (s1, s2, StringComparison.OrdinalIgnoreCase)

    (* List Extensions *)

    [<RequireQualifiedAccess>]
    module List =

        let chooseMaxBy projection =
                List.map (fun x -> x, projection x)
             >> List.choose (function | (x, Some y) -> Some (x, y) | _ -> None)
             >> List.sortBy (fun (_, y) -> y)
             >> List.map fst
             >> function | [] -> None | x :: _ -> Some x

    (* Charset *)

    [<RequireQualifiedAccess>]
    module Charset =

        (* Negotiation *)

        let private max (Charset s) =
            function | { AcceptableCharset.Charset = CharsetRange.Charset (Charset s') } when s == s' -> Some 0
                     | { Charset = CharsetRange.Any } -> Some 1
                     | _ -> None

        let private map requested =
            List.map (fun (x: Charset) ->
                x, List.chooseMaxBy (max x) requested)

        let private sort =
            List.sortBy (fun (_, y) ->
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
            function | Some (AcceptCharset x) -> Negotiated (run x supported)
                     | _ -> Free

        let negotiated acceptCharset supported =
                negotiate 
            <!> supported
            <*> acceptCharset

        (* Decisions *)

        let requested acceptCharset =
                Option.isSome 
            <!> acceptCharset

        let negotiable acceptCharset supported =
                function | Negotiated x when not (List.isEmpty x) -> true
                         | _ -> false
            <!> negotiated acceptCharset supported

    (* Encoding *)

    [<RequireQualifiedAccess>]
    module Encoding =

        // TODO: Better Content-Coding Negotiation - proper support of identity, etc.

        (* Negotiation *)

        let private max (ContentCoding c) =
            function | { AcceptableEncoding.Encoding = EncodingRange.Coding (ContentCoding c') } when c == c' -> Some 0
                     | { Encoding = EncodingRange.Any } -> Some 1
                     | _ -> None

        let private map requested =
            List.map (fun (x: ContentCoding) ->
                x, List.chooseMaxBy (max x) requested)

        let private sort =
            List.sortBy (fun (_, y) ->
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

        let negotiate supported =
            function | Some (AcceptEncoding x) -> Negotiated (run x supported)
                     | _ -> Free

        let negotiated acceptEncoding supported =
                negotiate
            <!> supported
            <*> acceptEncoding

        (* Decisions *)

        let requested acceptEncoding =
                Option.isSome 
            <!> acceptEncoding

        let negotiable acceptEncoding supported =
                function | Negotiated x when not (List.isEmpty x) -> true
                         | _ -> false
            <!> negotiated acceptEncoding supported

    (* Language *)

    [<RequireQualifiedAccess>]
    module Language =

        (* Note: This is intended to approximate the semantics
           of Basic Filtering as specified in Section 3.3.1 of RFC 4647.

           See [http://tools.ietf.org/html/rfc4647#section-3.3.1] *)

        (* Negotiation *)

        let private reify tag =
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
            Seq.zip (reify tag) >> Seq.forall ((<||) (==))

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
            function | Some (AcceptLanguage x) -> Negotiated (run supported x)
                     | _ -> Free

        let negotiated acceptLanguage supported =
                negotiate 
            <!> supported
            <*> acceptLanguage

        (* Decisions *)

        let requested acceptLanguage =
                Option.isSome 
            <!> acceptLanguage

        let negotiable acceptLanguage supported =
                function | Negotiated x when not (List.isEmpty x) -> true
                         | _ -> false
            <!> negotiated acceptLanguage supported

    (* MediaType *)

    [<RequireQualifiedAccess>]
    module MediaType =

        (* Negotiation *)

        let private max (MediaType (Type t, SubType s, _)) =
            function | { MediaRange = Closed (Type t', SubType s', _) } when t == t' && s == s' -> Some 0
                     | { MediaRange = MediaRange.Partial (Type t', _) } when t == t' -> Some 1
                     | { MediaRange = Open _ } -> Some 2
                     | _ -> None

        let private map requested =
            List.map (fun (x: MediaType) ->
                x, List.chooseMaxBy (max x) requested)

        let private sort =
            List.sortBy (fun (_, y) ->
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
            function | Some (Accept x) -> Negotiated (run x supported)
                     | _ -> Free

        let negotiated accept supported =
                negotiate 
            <!> supported
            <*> accept

        (* Decisions *)

        let requested accept =
                Option.isSome 
            <!> accept

        let negotiable accept supported =
                function | Negotiated x when List.isEmpty x = false -> true
                         | _ -> false
            <!> negotiated accept supported

(* Method *)

[<RequireQualifiedAccess>]
module Method =

    (* Decisions *)

    let known meth known =
            (fun x -> List.exists (fun x' -> x = x'))
        <!> meth
        <*> known

    let supported meth supported =
            (fun x -> List.exists (fun x' -> x = x'))
        <!> meth
        <*> supported

    let delete meth =
            (=) DELETE 
        <!> meth

    let getOrHead meth =
            flip Set.contains (set [ GET; HEAD ])
        <!> meth

    let options meth =
            (=) OPTIONS 
        <!> meth

    let patch meth =
            (=) (Method.Custom "PATCH")
        <!> meth

    let post meth =
            (=) POST 
        <!> meth

    let put meth =
            (=) PUT 
        <!> meth