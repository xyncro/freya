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

[<AutoOpen>]
module Freya.Types.Parsing

open FParsec

(* Parsing *)

let parse (p: Parse<'a>) s =
    match run p s with
    | Success (x, _, _) -> x
    | Failure (e, _, _) -> failwith e

let tryParse (p: Parse<'a>) s =
    match run p s with
    | Success (x, _, _) -> Some x
    | Failure (_, _, _) -> None

(* Common Parsers *)

let ampersandP : Parser<unit, unit> =
    skipChar '&'

let commaP : Parser<unit, unit> =
    skipChar ','

let semicolonP : Parser<unit, unit> =
    skipChar ';'

let slashP : Parser<unit, unit> =
    skipChar '/'

let spaceP : Parser<unit, unit> =
    skipChar ' '