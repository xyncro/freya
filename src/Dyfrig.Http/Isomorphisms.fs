module internal Dyfrig.Http.Isomorphisms

open System
open System.Globalization
open Aether

(* Constructors *)

let private iso p f =
    Parsers.parse p, Formatters.format f

let private isoP p f =
    Parsers.parseP p, Formatters.format f


module Generic =

    (* Box *)

    let boxIso<'a> : Iso<obj, 'a> =
        unbox<'a>, box

    (* DateTime *)

    let private dateTimeFormat =
        CultureInfo.InvariantCulture.DateTimeFormat

    let private dateTimeAdjustment =
        DateTimeStyles.AdjustToUniversal

    let dateTimePIso : PIso<string, DateTime> =
        (fun s ->
            match DateTime.TryParse (s, dateTimeFormat, dateTimeAdjustment) with
            | true, x -> Some x
            | _ -> None),
        (fun d -> d.ToString ("r"))

    (* Header
        
       Headers in OWIN dictionaries are represented by an array of strings, which
       we simply concatenate, as we expect a single string. This *shouldn't* cause
       problems, but suggestions for alternatives are welcome here. *)

    let headerIso =
        (fun s -> String.concat "," s),
        (fun s -> [| s |])

    (* Integer *)

    let intPIso : PIso<string, int> =
        (fun s ->
            match Int32.TryParse s with
            | true, x -> Some x
            | _ -> None),
        (string)

    (* Scheme *)

    let schemeIso =
        iso Parsers.Generic.scheme Formatters.Generic.scheme

    (* TimeSpan *)

    let timeSpanIso : Iso<int, TimeSpan> =
        (fun s -> TimeSpan.FromSeconds (float s)),
        (fun t -> t.Seconds)


module RFC7230 =

    // TODO: Query Iso

    (* Section 3 *)

    let methodIso =
        iso Parsers.RFC7230.meth Formatters.RFC7230.meth

    let httpVersionIso =
        iso Parsers.RFC7230.httpVersion Formatters.RFC7230.httpVersion

    (* Section 6 *)

    let connectionPIso =
        isoP Parsers.RFC7230.connection Formatters.RFC7230.connection


module RFC7231 =

    (* Section 5 *)

    let acceptPIso =
        isoP Parsers.RFC7231.accept Formatters.RFC7231.accept

    let acceptCharsetPIso =
        isoP Parsers.RFC7231.acceptCharset Formatters.RFC7231.acceptCharset

    let acceptEncodingPIso =
        isoP Parsers.RFC7231.acceptEncoding Formatters.RFC7231.acceptEncoding

    let acceptLanguagePIso =
        isoP Parsers.RFC7231.acceptLanguage Formatters.RFC7231.acceptLanguage


module RFC7232 =

    (* Section 2 *)

    let eTagPIso =
        isoP Parsers.RFC7232.eTag Formatters.RFC7232.eTag

    (* Section 3 *)

    let ifMatchPIso =
        isoP Parsers.RFC7232.ifMatch Formatters.RFC7232.ifMatch

    let ifNoneMatchPIso =
        isoP Parsers.RFC7232.ifNoneMatch Formatters.RFC7232.ifNoneMatch