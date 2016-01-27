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
open Arachne.Http
open Freya.Core
open Freya.Core.Operators

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
                fun x y -> (function | lm, Some (IfModifiedSince ms) -> lm > ms
                                     | _ -> false) (x, y)
            <!> Option.orElse (Freya.init DateTime.MinValue) lastModified
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
                fun x y -> (function | lm, Some (IfUnmodifiedSince us) -> lm < us
                                     | _ -> true) (x, y)
            <!> Option.orElse (Freya.init DateTime.MaxValue) lastModified
            <*> ifUnmodifiedSince

(* Content Negotiation *)

[<RequireQualifiedAccess>]
module ContentNegotiation =

    let private negotiated =
        function | Some x -> Negotiated x
                 | _ -> Free

    (* Charset *)

    [<RequireQualifiedAccess>]
    module Charset =

        let negotiate supported acceptable =
            Option.map (function | AcceptCharset x -> x) acceptable
            |> Charset.negotiate supported
            |> negotiated

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

    (* ContentCoding *)

    [<RequireQualifiedAccess>]
    module ContentCoding =

        let negotiate supported acceptable =
            Option.map (function | AcceptEncoding x -> x) acceptable
            |> ContentCoding.negotiate supported
            |> negotiated

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

    (* Obsolete

       To be removed in 4.0 *)

    [<Obsolete ("Use Content Coding instead.")>]
    [<RequireQualifiedAccess>]
    module Encoding =

        [<Obsolete ("Use ContentCoding.negotiate instead.")>]
        let negotiate =
            ContentCoding.negotiate

        [<Obsolete ("Use ContentCoding.negotiated instead.")>]
        let negotiated =
            ContentCoding.negotiated

        [<Obsolete ("Use ContentCoding.requested instead.")>]
        let requested =
            ContentCoding.requested

        [<Obsolete ("Use ContentCoding.negotiable instead.")>]
        let negotiable =
            ContentCoding.negotiable

    (* Language *)

    [<RequireQualifiedAccess>]
    module Language =

        let negotiate supported acceptable =
            Option.map (function | AcceptLanguage x -> x) acceptable
            |> Language.negotiate supported
            |> negotiated

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

        let negotiate supported acceptable =
            Option.map (function | Accept x -> x) acceptable
            |> MediaType.negotiate supported
            |> negotiated

        let negotiated accept supported =
                negotiate
            <!> supported
            <*> accept

        (* Decisions *)

        let requested accept =
                Option.isSome 
            <!> accept

        let negotiable accept supported =
                function | Negotiated x when not (List.isEmpty x) -> true
                         | _ -> false
            <!> negotiated accept supported

(* Method *)

[<RequireQualifiedAccess>]
module Method =

    (* Decisions *)

    let known meth known =
            fun x -> List.exists (fun x' -> x = x')
        <!> meth
        <*> known

    let supported meth supported =
            fun x -> List.exists (fun x' -> x = x')
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