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

open Freya.Types
open Freya.Types.Formatting
open Freya.Types.Parsing
open FParsec

(* Grammar *)

let private varchar =
    Set.unionMany [
        Grammar.alpha
        Grammar.digit
        set [ '_' ] ]

(* RFC 6570

   Types, parsers and formatters implemented to mirror the specification of 
   URI Template semantics as defined in RFC 6570.

   Taken from [http://tools.ietf.org/html/rfc6570] *)

(* Template

   Taken from RFC 6570, Section 2 Syntax
   See [http://tools.ietf.org/html/rfc6570#section-2] *)

type UriTemplate =
    | UriTemplate of UriTemplatePart list

and UriTemplatePart =
    | Literal of string
    | Expression of Operator option * VariableList

(* Operators

   Taken from RFC 6570, Section 2 Expressions
   See [http://tools.ietf.org/html/rfc6570#section-2.2] *)

and Operator =
    | Level2 of OperatorLevel2
    | Level3 of OperatorLevel3
    | Reserved of OperatorReserved

    static member TypeMapping =

        let operatorP =
            choice [
                OperatorLevel2.TypeMapping.Parse |>> Level2
                OperatorLevel3.TypeMapping.Parse |>> Level3
                OperatorReserved.TypeMapping.Parse |>> Reserved ]

        let operatorF =
            function | Level2 o -> OperatorLevel2.TypeMapping.Format o
                     | Level3 o -> OperatorLevel3.TypeMapping.Format o
                     | Reserved o -> OperatorReserved.TypeMapping.Format o

        { Parse = operatorP
          Format = operatorF }

and OperatorLevel2 =
    | Plus
    | Hash

    static member TypeMapping =

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

    static member TypeMapping =

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

    static member TypeMapping =

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

    static member TypeMapping =

        let variableListP =
            sepBy1 VariableSpec.TypeMapping.Parse (skipChar ',')
            |>> VariableList

        let variableListF =
            function | VariableList v -> join VariableSpec.TypeMapping.Format commaF v

        { Parse = variableListP
          Format = variableListF }

and VariableSpec =
    | VariableSpec of VariableName * Modifier option

    static member TypeMapping =

        let variableSpecP =
            VariableName.TypeMapping.Parse .>>. opt Modifier.TypeMapping.Parse
            |>> VariableSpec

        let variableSpecF =
            function | VariableSpec (name, Some m) -> VariableName.TypeMapping.Format name >> Modifier.TypeMapping.Format m
                     | VariableSpec (name, _) -> VariableName.TypeMapping.Format name

        { Parse = variableSpecP
          Format = variableSpecF }

and VariableName =
    | VariableName of string

    static member TypeMapping =

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

    static member TypeMapping =

        let modifierP =
            ModifierLevel4.TypeMapping.Parse |>> Level4

        let modifierF =
            function | Level4 m -> ModifierLevel4.TypeMapping.Format m

        { Parse = modifierP
          Format = modifierF }

and ModifierLevel4 =
    | Prefix of int
    | Explode

    static member TypeMapping =

        let modifierLevel4P =
            choice [
                skipChar ':' >>. pint32 |>> Prefix
                skipChar '*' >>% Explode ]

        let modifierLevel4F =
            function | Prefix i -> appendf1 ":{0}" i
                     | Explode -> append "*"

        { Parse = modifierLevel4P
          Format = modifierLevel4F }