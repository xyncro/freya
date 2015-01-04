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
//----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module internal Freya.Types.Grammar

(* RFC 5234

   Core ABNF grammar rules as defined in RFC 5234.

   Taken from RFC 5234, Appendix B.1 Core Rules
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

let hexdig =
    Set.unionMany [
        digit
        set [ 'A'; 'B'; 'C'; 'D'; 'E'; 'F'
              'a'; 'b'; 'c'; 'd'; 'e'; 'f' ] ]

let wsp = 
    set [ sp; htab ]