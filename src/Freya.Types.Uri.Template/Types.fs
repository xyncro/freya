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
//
//----------------------------------------------------------------------------

module Freya.Types.Uri.Template

open System.Text
open Freya.Types
open Freya.Types.Formatting
open Freya.Types.Parsing
open Freya.Types.Uri
open FParsec

(* Encoding

   Logic to perform percent-encoding/decoding of data within URIs
   given appropriate whitelists of characters which should be left
   unencoded (this supports the different encodings required for
   various expansion modes of URI Templates). *)

// TODO: Find a better home this kind of encoding, probably somewhere
// less specific within the Freya.Types.* hierarchy...

[<RequireQualifiedAccess>]
module internal Encoding =

    (* Grammar *)

    let private pct =
        byte 0x25

    (* UTF-8

       Shorthand for UTF-8 encoding and decoding of strings (given
       the assumption that the .NET UTF-16/Unicode string is our
       basic string type). *)

    let private toBytes : string -> byte list =
        Encoding.UTF8.GetBytes >> List.ofArray

    let private toString : byte list -> string =
        List.toArray >> Encoding.UTF8.GetString

    (* Indices

       Simple lookups/indices for converting between bytes and the hex
       encoding of those bytes. *)

    let private hex =
        [ 0x00 .. 0xff ]
        |> List.map byte
        |> List.map (fun i -> i, toBytes (i.ToString "X2"))

    let private hexI =
        hex
        |> Map.ofList

//    let private byteI =
//        hex
//        |> List.map (fun (a, b) -> (b, a))
//        |> Map.ofList

    (* Encoding

       Encoding functions, providing a function to create an encoder
       given a whitelist set of allowed characters within the encoded
       output. *)

    // TODO: Allow for pct encoded characters to be skipped rather
    // than re-encoded in cases where that is valid.

    let private encode res =
        let rec enc r =
            function | [] -> r
                     | h :: t when Set.contains h res -> enc (r @ [ h ]) t
                     | h :: t -> enc (r @ [ pct ] @ Map.find h hexI) t

        enc []

    let makePctEncode res =
        let res = Set.map byte res

        toBytes >> encode res >> toString

    (* Decoding
    
       Decoding functions, providing a simple function to decode
       a percent-encoded string to a .NET native UTF-16/Unicode string. *)

//    let private decode =
//        let rec dec r =
//            function | [] -> r
//                     | h :: x :: y :: t when h = pct -> dec (r @ [ Map.find [ x; y ] byteI ]) t
//                     | h :: t -> dec (r @ [ h ]) t
//
//        dec []

//    let pctDecode =
//        toBytes >> decode >> toString

(* RFC 6570

   Types, parsers and formatters implemented to mirror the specification of 
   URI Template semantics as defined in RFC 6570.

   Taken from [http://tools.ietf.org/html/rfc6570] *)

(* Grammar

   NOTE: We do not currently support either PCT encoded characters (a
   must fix) or international characters (supporting IRIs - this may
   be supported in future). *)

let private literal =
    Set.unionMany [
        set [ char 0x21 ]
        charRange 0x23 0x24
        set [ char 0x26 ]
        charRange 0x28 0x3b
        set [ char 0x3d ]
        charRange 0x3f 0x5b
        set [ char 0x5d ]
        set [ char 0x5f ]
        charRange 0x61 0x7a
        set [ char 0x7e ] ]

let private varchar =
    Set.unionMany [
        Grammar.alpha
        Grammar.digit
        set [ '_' ] ]

(* Template

   Taken from RFC 6570, Section 2 Syntax
   See [http://tools.ietf.org/html/rfc6570#section-2] *)

type UriTemplate =
    | UriTemplate of UriTemplatePart list

    static member Mapping =

        let uriTemplateP =
            many1 UriTemplatePart.Mapping.Parse |>> UriTemplate

        let uriTemplateF =
            function | UriTemplate u -> join UriTemplatePart.Mapping.Format id u

        { Parse = uriTemplateP
          Format = uriTemplateF }

    static member Rendering =

        let uriTemplateR (data: UriTemplateData) =
            function | UriTemplate p -> join (UriTemplatePart.Rendering.Render data) id p

        { Render = uriTemplateR }

    static member Format =
        Formatting.format UriTemplate.Mapping.Format

    static member Parse =
        Parsing.parse UriTemplate.Mapping.Parse

    static member TryParse =
        Parsing.tryParse UriTemplate.Mapping.Parse

    override x.ToString () =
        UriTemplate.Format x

    member x.Render data =
        Rendering.render UriTemplate.Rendering.Render data x

and UriTemplatePart =
    | Literal of Literal
    | Expression of Expression

    static member Mapping =

        let uriTemplatePartP =
            (Expression.Mapping.Parse |>> Expression) <|> (Literal.Mapping.Parse |>> Literal)

        let uriTemplatePartF =
            function | Literal l -> Literal.Mapping.Format l
                     | Expression e -> Expression.Mapping.Format e

        { Parse = uriTemplatePartP
          Format = uriTemplatePartF }

    static member Rendering =

        let uriTemplatePartR data =
            function | Literal l -> Literal.Rendering.Render data l
                     | Expression e-> Expression.Rendering.Render data e

        { Render = uriTemplatePartR }

and Literal =
    | Literal of string

    static member Mapping =

        let literalP =
            many1Satisfy ((?>) literal) |>> Literal.Literal

        let literalF =
            function | Literal l -> append l

        { Parse = literalP
          Format = literalF }

    static member Rendering =

        let literalR _ =
            function | Literal l -> append l

        { Render = literalR }

and Expression =
    | Expression of Operator option * VariableList

    static member Mapping =

        let expressionP =
            between 
                (skipChar '{') (skipChar '}') 
                (opt Operator.Mapping.Parse .>>. VariableList.Mapping.Parse)
                |>> Expression

        let expressionF =
            function | Expression (Some o, v) ->
                           append "{"
                        >> Operator.Mapping.Format o
                        >> VariableList.Mapping.Format v
                        >> append "}"
                     | Expression (_, v) ->
                           append "{"
                        >> VariableList.Mapping.Format v
                        >> append "}"

        { Parse = expressionP
          Format = expressionF }

    static member Rendering =

        (* Encoding

           The two forms of encoding (unreserved only, i.e.
           simple expansion) and unreserved/reserved for
           reserved string expansion. *)

        let reserved =
            Set.unionMany [
                unreserved
                reserved ]

        let reservedEncoding =
            Encoding.makePctEncode reserved

        let standardEncoding =
            Encoding.makePctEncode unreserved

        let variableSpecR encode (UriTemplateData data) =
            function | VariableSpec (VariableName n, _) ->
                        match Map.tryFind n data with
                        | Some (Atom a) -> append (encode a)
                        | _ -> id

        let simpleStringExpansion (VariableList v) data =
            join (variableSpecR standardEncoding data) commaF v

        let reservedStringExpansion (VariableList v) data =
            join (variableSpecR reservedEncoding data) commaF v

        let expressionR data =
            function | Expression (None, v) -> simpleStringExpansion v data
                     | Expression (Some (Level2 Plus), v) -> reservedStringExpansion v data
                     | Expression (Some (Level2 Hash), _) -> id
                     | Expression (Some (Level3 Dot), _) -> id
                     | Expression (Some (Level3 Slash), _) -> id
                     | Expression (Some (Level3 SemiColon), _) -> id
                     | Expression (Some (Level3 Question), _) -> id
                     | Expression (Some (Level3 Ampersand), _) -> id
                     | _ -> id

        { Render = expressionR }

(* Operators

   Taken from RFC 6570, Section 2.2 Expressions
   See [http://tools.ietf.org/html/rfc6570#section-2.2] *)

and Operator =
    | Level2 of OperatorLevel2
    | Level3 of OperatorLevel3
    | Reserved of OperatorReserved

    static member Mapping =

        let operatorP =
            choice [
                OperatorLevel2.Mapping.Parse |>> Level2
                OperatorLevel3.Mapping.Parse |>> Level3
                OperatorReserved.Mapping.Parse |>> Reserved ]

        let operatorF =
            function | Level2 o -> OperatorLevel2.Mapping.Format o
                     | Level3 o -> OperatorLevel3.Mapping.Format o
                     | Reserved o -> OperatorReserved.Mapping.Format o

        { Parse = operatorP
          Format = operatorF }

and OperatorLevel2 =
    | Plus
    | Hash

    static member Mapping =

        let operatorLevel2P =
            choice [
                skipChar '+' >>% Plus
                skipChar '#' >>% Hash ]

        let operatorLevel2F =
            function | Plus -> append "+"
                     | Hash -> append "#"

        { Parse = operatorLevel2P
          Format = operatorLevel2F }

and OperatorLevel3 =
    | Dot
    | Slash
    | SemiColon
    | Question
    | Ampersand

    static member Mapping =

        let operatorLevel3P =
            choice [
                skipChar '.' >>% Dot
                skipChar '/' >>% Slash
                skipChar ';' >>% SemiColon
                skipChar '?' >>% Question
                skipChar '&' >>% Ampersand ]

        let operatorLevel3F =
            function | Dot -> append "."
                     | Slash -> append "/"
                     | SemiColon -> append ";"
                     | Question -> append "?"
                     | Ampersand -> append "&"

        { Parse = operatorLevel3P
          Format = operatorLevel3F }

and OperatorReserved =
    | Equals
    | Comma
    | Exclamation
    | At
    | Pipe

    static member Mapping =

        let operatorReservedP =
            choice [
                skipChar '=' >>% Equals
                skipChar ',' >>% Comma
                skipChar '!' >>% Exclamation
                skipChar '@' >>% At
                skipChar '|' >>% Pipe ]

        let operatorReservedF =
            function | Equals -> append "="
                     | Comma -> append ","
                     | Exclamation -> append "!"
                     | At -> append "@"
                     | Pipe -> append "!"

        { Parse = operatorReservedP
          Format = operatorReservedF }

(* Variables

   Taken from RFC 6570, Section 2.3 Variables
   See [http://tools.ietf.org/html/rfc6570#section-2.3] *)

and VariableList =
    | VariableList of VariableSpec list

    static member Mapping =

        let variableListP =
            sepBy1 VariableSpec.Mapping.Parse (skipChar ',')
            |>> VariableList

        let variableListF =
            function | VariableList v -> join VariableSpec.Mapping.Format commaF v

        { Parse = variableListP
          Format = variableListF }

and VariableSpec =
    | VariableSpec of VariableName * Modifier option

    static member Mapping =

        let variableSpecP =
            VariableName.Mapping.Parse .>>. opt Modifier.Mapping.Parse
            |>> VariableSpec

        let variableSpecF =
            function | VariableSpec (name, Some m) ->
                           VariableName.Mapping.Format name
                        >> Modifier.Mapping.Format m
                     | VariableSpec (name, _) ->
                        VariableName.Mapping.Format name

        { Parse = variableSpecP
          Format = variableSpecF }

and VariableName =
    | VariableName of string

    static member Mapping =

        let varcharsP =
            many1Satisfy ((?>) varchar)

        let variableNamePartP =
            opt (pstring ".") .>>. varcharsP
            |>> function | (Some s1, s2) -> s1 + s2
                         | (_, s2) -> s2

        let variableNameP =
            varcharsP .>>. many variableNamePartP
            |>> (fun (c, s) ->
                (String.concat "" >> VariableName) (c :: s))

        let variableNameF =
            function | VariableName n -> append n

        { Parse = variableNameP
          Format = variableNameF }

(* Modifiers

   Taken from RFC 6570, Section 2.4 Value Modifiers
   See [http://tools.ietf.org/html/rfc6570#section-2.4] *)

and Modifier =
    | Level4 of ModifierLevel4

    static member Mapping =

        let modifierP =
            ModifierLevel4.Mapping.Parse |>> Level4

        let modifierF =
            function | Level4 m -> ModifierLevel4.Mapping.Format m

        { Parse = modifierP
          Format = modifierF }

and ModifierLevel4 =
    | Prefix of int
    | Explode

    static member Mapping =

        let modifierLevel4P =
            choice [
                skipChar ':' >>. pint32 |>> Prefix
                skipChar '*' >>% Explode ]

        let modifierLevel4F =
            function | Prefix i -> appendf1 ":{0}" i
                     | Explode -> append "*"

        { Parse = modifierLevel4P
          Format = modifierLevel4F }

(* Data

   Types representing data which may be rendered or extracted
   using UriTemplates. *)

and UriTemplateData =
    | UriTemplateData of Map<string, UriTemplateDataItem>

and UriTemplateDataItem =
    | Atom of string
    | List of string list
    | Map of Map<string, string>