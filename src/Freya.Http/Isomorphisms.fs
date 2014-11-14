module internal Freya.Http.Isomorphisms

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

    (* Header
        
       Headers in OWIN dictionaries are represented by an array of strings, which
       we simply concatenate, as we expect a single string. This *shouldn't* cause
       problems, but suggestions for alternatives are welcome here. *)

    let headerIso =
        (fun s -> String.concat "," s),
        (fun s -> [| s |])

    (* Scheme *)

    let schemeIso =
        iso Parsers.Generic.scheme Formatters.Generic.scheme


module RFC7230 =

    // TODO: Query Iso

    (* Section 3 *)

    let methodIso =
        iso Parsers.RFC7230.meth Formatters.RFC7230.meth

    let httpVersionIso =
        iso Parsers.RFC7230.httpVersion Formatters.RFC7230.httpVersion

    let contentLengthPIso =
        isoP Parsers.RFC7230.contentLength Formatters.RFC7230.contentLength

    (* Section 6 *)

    let connectionPIso =
        isoP Parsers.RFC7230.connection Formatters.RFC7230.connection


module RFC7231 =

    (* Section 3 *)

    let contentEncodingPIso =
        isoP Parsers.RFC7231.contentEncoding Formatters.RFC7231.contentEncoding

    let contentTypePIso =
        isoP Parsers.RFC7231.contentType Formatters.RFC7231.contentType

    let expectPIso =
        isoP Parsers.RFC7231.expect Formatters.RFC7231.expect

    let maxForwardsPIso =
        isoP Parsers.RFC7231.maxForwards Formatters.RFC7231.maxForwards

    (* Section 5 *)

    let acceptPIso =
        isoP Parsers.RFC7231.accept Formatters.RFC7231.accept

    let acceptCharsetPIso =
        isoP Parsers.RFC7231.acceptCharset Formatters.RFC7231.acceptCharset

    let acceptEncodingPIso =
        isoP Parsers.RFC7231.acceptEncoding Formatters.RFC7231.acceptEncoding

    let acceptLanguagePIso =
        isoP Parsers.RFC7231.acceptLanguage Formatters.RFC7231.acceptLanguage

    (* Section 7 *)

    let datePIso =
        isoP Parsers.RFC7231.date Formatters.RFC7231.date

    let retryAfterPIso =
        isoP Parsers.RFC7231.retryAfter Formatters.RFC7231.retryAfter

    let allowPIso =
        isoP Parsers.RFC7231.allow Formatters.RFC7231.allow


module RFC7232 =

    (* Section 2 *)

    let lastModifiedPIso =
        isoP Parsers.RFC7232.lastModified Formatters.RFC7232.lastModified

    let eTagPIso =
        isoP Parsers.RFC7232.eTag Formatters.RFC7232.eTag

    (* Section 3 *)

    let ifMatchPIso =
        isoP Parsers.RFC7232.ifMatch Formatters.RFC7232.ifMatch

    let ifNoneMatchPIso =
        isoP Parsers.RFC7232.ifNoneMatch Formatters.RFC7232.ifNoneMatch

    let ifModifiedSincePIso =
        isoP Parsers.RFC7232.ifModifiedSince Formatters.RFC7232.ifModifiedSince

    let ifUnmodifiedSincePIso =
        isoP Parsers.RFC7232.ifUnmodifiedSince Formatters.RFC7232.ifUnmodifiedSince


module RFC7234 =

    (* Section 5 *)

    let agePIso =
        isoP Parsers.RFC7234.age Formatters.RFC7234.age

    let expiresPIso =
        isoP Parsers.RFC7234.expires Formatters.RFC7234.expires
