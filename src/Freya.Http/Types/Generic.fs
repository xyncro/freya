[<AutoOpen>]
module Freya.Http.Generic

#nowarn "60"

open FParsec

(* Generic *)

let internal commaP : Parser<unit, unit> =
    skipChar ','

let internal semicolonP : Parser<unit, unit> =
    skipChar ';'

(* Scheme *)

type Scheme =
    | HTTP 
    | HTTPS 
    | Custom of string

let private schemeF =
    function | HTTP -> append "http"
             | HTTPS -> append "https"
             | Scheme.Custom x -> append x

let private schemeP =
    choice [
        skipStringCI "http" >>% HTTP
        skipStringCI "https" >>% HTTPS
        restOfLine false |>> Scheme.Custom ]

type Scheme with

    static member Format =
        Formatting.format schemeF

    static member Parse =
        Parsing.parse schemeP

    override x.ToString () =
        Scheme.Format x