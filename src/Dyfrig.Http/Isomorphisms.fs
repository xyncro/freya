module internal Dyfrig.Http.Isomorphisms

(* Constructors *)

let private iso p f =
    Parsers.parse p, Formatters.format f

let private isoP p f =
    Parsers.parseP p, Formatters.format f


module Generic =

    let schemeIso =
        iso Parsers.Generic.scheme Formatters.Generic.scheme


module RFC7230 =

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