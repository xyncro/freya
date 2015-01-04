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
module internal Freya.Types.Formatting

open System.Text

(* Formatting *)

let format (formatter: 'a -> StringBuilder -> StringBuilder) =
    fun a -> string (formatter a (StringBuilder ()))

(* Helpers *)

type Formatter<'a> =
    'a -> StringBuilder -> StringBuilder

type Separator =
    StringBuilder -> StringBuilder

let append (s: string) (b: StringBuilder) =
    b.Append s

let appendf1 (s: string) (v1: obj) (b: StringBuilder) =
    b.AppendFormat (s, v1)

let appendf2 (s: string) (v1: obj) (v2: obj) (b: StringBuilder) =
    b.AppendFormat (s, v1, v2)

let join<'a> (f: Formatter<'a>) (s: Separator) =
    let rec join values (b: StringBuilder) =
        match values with
        | [] -> b
        | h :: [] -> f h b
        | h :: t -> (f h >> s >> join t) b

    join

(* Common *)

let ampersandF : Separator =
    append "&"

let commaF : Separator =
    append ","

let semicolonF : Separator =
    append ";"

let slashF : Separator =
    append "/"

let spaceF : Separator =
    append " "
