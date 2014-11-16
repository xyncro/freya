[<AutoOpen>]
module internal Freya.Http.RFC5234

(* RFC 5234

   Core grammar rules as defined in RFC 5234.

   Taken from RFC 5234, Appendix B.1. Core Rules
   See [http://tools.ietf.org/html/rfc5234#appendix-B.1] *)

let alpha = 
    Set.unionMany [ 
        charRange 0x41 0x5a
        charRange 0x61 0x7a ]

let digit = 
    charRange 0x30 0x39

let dquote = 
    char 0x22

let htab = 
    char 0x09

let sp = 
    char 0x20

let vchar =
    charRange 0x21 0x7e

let wsp = 
    set [ sp; htab ]