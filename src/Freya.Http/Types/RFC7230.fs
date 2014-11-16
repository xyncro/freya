[<AutoOpen>]
module Freya.Http.RFC7230

#nowarn "60"

open System
open FParsec

(* RFC 7230

   Types, parsers and formatters implemented to mirror the specification of 
   HTTP semantics as defined in RFC 7230.

   Taken from [http://tools.ietf.org/html/rfc7230] *)

(* Whitespace

   Taken from RFC 7230, Section 3.2.3. Whitespace
   See [http://tools.ietf.org/html/rfc7230#section-3.2.3] *)

let internal owsP = 
    skipManySatisfy ((?>) wsp)

//    let rws =
//        skipMany1Satisfy (fun c -> Set.contains c wsp)

//    let bws =
//        ows

(* Field Value Components

   Taken from RFC 7230, Section 3.2.6. Field Value Components
   See [http://tools.ietf.org/html/rfc7230#section-3.2.6] *)

let internal tchar = 
    Set.unionMany [ 
        set [ '!'; '#'; '$'; '%'; '&'; '\''; '*'
              '+'; '-'; '.'; '^'; '_'; '`'; '|'; '~' ]
        alpha
        RFC5234.digit ]

let internal tokenP = 
    many1Satisfy ((?>) tchar)

let internal obsText =
    charRange 0x80 0xff

let internal qdtext =
    Set.unionMany [
        set [ htab; sp; char 0x21 ]
        charRange 0x23 0x5b
        charRange 0x5d 0x7e
        obsText ]

//let ctext =
//    Set.unionMany [
//        set [ htab; sp ]
//        charRange 0x21 0x27
//        charRange 0x2a 0x5b
//        charRange 0x5d 0x7e
//        obsText ]

let internal quotedPairChars =
    Set.unionMany [
        set [ htab; sp ]
        vchar
        obsText ]

let internal quotedPairP : Parser<char, unit> =
        skipChar '\\' 
    >>. satisfy ((?>) quotedPairChars)

let internal quotedStringP : Parser<string, unit> =
        skipChar dquote 
    >>. many (quotedPairP <|> satisfy ((?>) qdtext)) |>> (fun x -> string (String (List.toArray x)))
    .>> skipChar dquote

(* ABNF List Extension: #rule

   Taken from RFC 7230, Section 7. ABNF List Extension: #rule
   [http://tools.ietf.org/html/rfc7230#section-7] *)

let private infixHeadP s p =
    (attempt p |>> Some) <|> (s >>% None)

let private infixTailP s p =
    many (owsP >>? s >>? owsP >>? opt p)

(* Note:
   The infix and prefix parsers are designed to convey as accurately as possible the 
   meaning of the ABNF #rule extension including the laxity of specification for backward 
   compatibility. Whether they are a perfectly true representation is open to debate, 
   but they should perform sensibly under normal conditions. *)

let internal infixP s p = 
    infixHeadP s p .>>. infixTailP s p .>> owsP |>> fun (x, xs) -> x :: xs |> List.choose id

let internal infix1P s p =
    notEmpty (infixP s p)

let internal prefixP s p =
    many (owsP >>? s >>? owsP >>? p)

(* HTTP Version

   Taken from RFC 7230, Section 3.1 Request Line
   See [http://tools.ietf.org/html/rfc7230#section-3.1] *)

type HttpVersion =
    | HTTP of float 
    | Custom of string

let private httpVersionF =
    function | HttpVersion.HTTP x -> appendf1 "HTTP/{0:G4}" x 
             | HttpVersion.Custom x -> append x

let private httpVersionP =
    choice [
        skipString "HTTP/1.0" >>% HttpVersion.HTTP 1.0
        skipString "HTTP/1.1" >>% HttpVersion.HTTP 1.1
        restOfLine false |>> HttpVersion.Custom ]

type HttpVersion with

    static member Format =
        Formatting.format httpVersionF

    static member Parse =
        Parsing.parse httpVersionP

    override x.ToString () =
        HttpVersion.Format x

(* Content-Length

   Taken from RFC 7230, Section 3.3.2 Content-Length
   See [http://tools.ietf.org/html/rfc7230#section-3.3.2] *)

type ContentLength =
    | ContentLength of int

let private contentLengthF =
    function | ContentLength x -> append (string x)

let private contentLengthP =
    puint32 |>> (int >> ContentLength)

type ContentLength with

    static member Format =
        Formatting.format contentLengthF

    static member TryParse =
        Parsing.parseP contentLengthP

    override x.ToString () =
        ContentLength.Format x

(* Connection

   Taken from RFC 7230, Section 6.1 Connection
   See [http://tools.ietf.org/html/rfc7230#section-6.1] *)

type Connection =
    | Connection of ConnectionOption list

and ConnectionOption =
    | ConnectionOption of string

let private connectionOptionF =
    function | ConnectionOption x -> append x

let private connectionF =
    function | Connection x -> join connectionOptionF commaF x

let private connectionP =
    infix1P (skipChar ',') tokenP |>> (List.map ConnectionOption >> Connection)

type Connection with

    static member Format =
        Formatting.format connectionF

    static member TryParse =
        Parsing.parseP connectionP

    override x.ToString () =
        Connection.Format x