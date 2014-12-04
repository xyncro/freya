[<AutoOpen>]
module Freya.Types.Cors.Types

#nowarn "60"

open FParsec
open Freya.Types
open Freya.Types.Http
open Freya.Types.Uri

(* RFC 6454

   Types, parsers and formatters implemented to mirror the specification of 
   Origin semantics as defined in RFC 6454.

   Taken from [http://tools.ietf.org/html/rfc6454] *)

type Origin =
    | Origin of OriginListOrNull

and OriginListOrNull =
    | Origins of SerializedOrigin list
    | Null

and SerializedOrigin =
    { Scheme: Scheme
      Host: Host
      Port: Port option }

(* Formatting *)

let private serializedOriginF =
    function | { Scheme = s
                 Host = h
                 Port = p } ->
                    let formatters =
                        [ schemeF s
                          hostF h
                          (function | Some p -> portF p | _ -> id) p ]

                    fun b -> List.fold (fun b f -> f b) b formatters

let private originF =
    function | Origin (Origins x) -> join spaceF serializedOriginF x
             | Origin (Null) -> append "null"

(* Parsing *)

let private serializedOriginP =
    schemeP .>> skipString "://" .>>. hostP .>>. opt portP
    |>> fun ((scheme, host), port) ->
        { Scheme = scheme
          Host = host
          Port = port }

let private originP =
    choice [
        skipString "null" >>% Origin (Null)
        infix1P spaceP serializedOriginP |>> (Origins >> Origin) ]

(* Augmentation *)

type Origin with

    static member Format =
        format originF

    static member Parse =
        parseExact originP

    static member TryParse =
        parseOption originP

    override x.ToString () =
        Origin.Format x

(* W3C Recommendation - CORS

   Types, parsers and formatters implemented to mirror the specification of 
   CORS semantics as defined in W3C Recommendation on CORS (version dated 20140116).

   Taken from [http://www.w3.org/TR/2014/REC-cors-20140116] *)