module Freya.Types.Uri.Templates.Tests

open NUnit.Framework
open Freya.Types.Uri.Template
open Swensen.Unquote

(* Data

   Common superset of data items used throughout RFC 6570
   to frame examples. *)

let data =
    UriTemplateData (
        Map.ofList [
            [ "count "],      List [ "one"; "two"; "three" ]
            [ "dom" ],        List [ "example"; "com" ]
            [ "dub" ],        Atom "me/too"
            [ "hello" ],      Atom "Hello World!"
            [ "half" ],       Atom "50%"
            [ "var" ],        Atom "value"
            [ "who" ],        Atom "Fred"
            [ "base" ],       Atom "http://example.com/home/"
            [ "path" ],       Atom "/foo/bar"
            [ "list" ],       List [ "red"; "green"; "blue" ]
            [ "keys" ],       Keys [ ("semi", ";"); ("dot", "."); ("comma", ",") ]
            [ "v" ],          Atom "6"
            [ "x" ],          Atom "1024"
            [ "y" ],          Atom "768"
            [ "empty" ],      Atom ""
            [ "empty_keys" ], Keys [] ])

let (=?) str1 str2 =
    UriTemplate.Parse(str1).Render(data) =? str2

(* Illustrative Examples

   Examples used as part of the overview of URI Templates,
   giving a non-exhaustive flavour of URI Template expansion
   and variables/operators/modifiers. *)

[<Test>]
let ``Level 1 Examples Render Correctly`` () =

    (* Simple String Expansion *)

    "{var}" =? "value"
    "{hello}" =? "Hello%20World%21"

[<Test>]
let ``Level 2 Examples Render Correctly`` () =

    (* Reserved String Expansion *)

    "{+var}" =? "value"
    "{+hello}" =? "Hello%20World!"
    "{+path}/here" =? "/foo/bar/here"
    "here?ref={+path}" =? "here?ref=/foo/bar"

    (* Fragment Expansion *)

    "X{#var}" =? "X#value"
    "X{#hello}" =? "X#Hello%20World!"

(* Specifcation Examples

   Examples given as part of individual specification sections
   to drive the full set of expansion/modification cases, forming
   a specification by example in addition to the grammars and
   behaviours given as part of the specification. *)

[<Test>]
let ``Simple String Expansion Renders Correctly`` () =

    (* Simple String Expansion *)

    "{var}" =? "value"
    "{hello}" =? "Hello%20World%21"
    "{half}" =? "50%25"
    "O{empty}X" =? "OX"
    "O{undef}X" =? "OX"
    "{x,y}" =? "1024,768"
    "{x,hello,y}" =? "1024,Hello%20World%21,768"
    "?{x,empty}" =? "?1024,"
    //"?{x,undef}" =? "?1204"