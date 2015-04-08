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
            Key "count",      List [ "one"; "two"; "three" ]
            Key "dom",        List [ "example"; "com" ]
            Key "dub",        Atom "me/too"
            Key "he.llo",     Atom "Hello World!"
            Key "half",       Atom "50%"
            Key "var",        Atom "value"
            Key "who",        Atom "fred"
            Key "base",       Atom "http://example.com/home/"
            Key "path",       Atom "/foo/bar"
            Key "list",       List [ "red"; "green"; "blue" ]
            Key "keys",       Keys [ ("semi", ";"); ("dot", "."); ("comma", ",") ]
            Key "v",          Atom "6"
            Key "x",          Atom "1024"
            Key "y",          Atom "768"
            Key "empty",      Atom ""
            Key "empty_keys", Keys [] ])

let testMatch uri path data =
    UriTemplate.Parse(uri).Match(path) =? UriTemplateData (Map.ofList data)

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
    "{he.llo}" =? "Hello%20World%21"

[<Test>]
let ``Level 2 Examples Render Correctly`` () =

    (* Reserved String Expansion *)

    "{+var}" =? "value"
    "{+he.llo}" =? "Hello%20World!"
    "{+path}/here" =? "/foo/bar/here"
    "here?ref={+path}" =? "here?ref=/foo/bar"

    (* Fragment Expansion *)

    "X{#var}" =? "X#value"
    "X{#he.llo}" =? "X#Hello%20World!"

(* Specifcation Examples

   Examples given as part of individual specification sections
   to drive the full set of expansion/modification cases, forming
   a specification by example in addition to the grammars and
   behaviours given as part of the specification. *)

[<Test>]
let ``Simple Expansion Renders Correctly`` () =
    "{var}" =? "value"
    "{he.llo}" =? "Hello%20World%21"
    "{half}" =? "50%25"
    "O{empty}X" =? "OX"
    "O{undef}X" =? "OX"
    "{x,y}" =? "1024,768"
    "{x,he.llo,y}" =? "1024,Hello%20World%21,768"
    "?{x,empty}" =? "?1024,"
    "?{x,undef}" =? "?1024"
    "?{undef,y}" =? "?768"
    "{var:3}" =? "val"
    "{var:30}" =? "value"
    "{list}" =? "red,green,blue"
    "{list*}" =? "red,green,blue"
    "{keys}" =? "semi,%3B,dot,.,comma,%2C"
    "{keys*}" =? "semi=%3B,dot=.,comma=%2C"

[<Test>]
let ``Simple Matching Matches Correctly`` () =
    testMatch "/test/{atom}" "/test/one" [ Key "atom", Atom "one" ]
    testMatch "/test/{list*}" "/test/one,two,three" [ Key "list", List [ "one"; "two"; "three" ] ]
    testMatch "/test/{keys*}" "/test/one=a,two=b" [ Key "keys", Keys [ ("one", "a"); ("two", "b") ] ]

[<Test>]
let ``Reserved Expansion Renders Correctly`` () =
    "{+var}" =? "value"
    "{+he.llo}" =? "Hello%20World!"
    "{+half}" =? "50%25"
    "{base}index" =? "http%3A%2F%2Fexample.com%2Fhome%2Findex"
    "{+base}index" =? "http://example.com/home/index"
    "O{+empty}X" =? "OX"
    "O{+undef}X" =? "OX"
    "{+path}/here" =? "/foo/bar/here"
    "here?ref={+path}" =? "here?ref=/foo/bar"
    "up{+path}{var}/here" =? "up/foo/barvalue/here"
    "{+x,he.llo,y}" =? "1024,Hello%20World!,768"
    "{+path,x}/here" =? "/foo/bar,1024/here"
    "{+path:6}/here" =? "/foo/b/here"
    "{+list}" =? "red,green,blue"
    "{+list*}" =? "red,green,blue"
    "{+keys}" =? "semi,;,dot,.,comma,,"
    "{+keys*}" =? "semi=;,dot=.,comma=,"

// Note - this may seem incorrect, but is actually expected behaviour.
// These are not ideal operators to use for matching complex data!

[<Test>]
let ``Reserved Matching Matches Correctly`` () =
    testMatch "/test/{+atom}" "/test/one!" [ Key "atom", Atom "one!" ]
    testMatch "/test/{+list*}" "/test/one,two,three" [ Key "list", List [ "one,two,three" ] ]
    testMatch "/test/{+keys*}" "/test/one=a,two=b" [ Key "keys", List [ "one=a,two=b" ] ]

[<Test>]
let ``Fragment Expansion Renders Correctly`` () =
    "{#var}" =? "#value"
    "{#he.llo}" =? "#Hello%20World!"
    "{#half}" =? "#50%25"
    "foo{#empty}" =? "foo#"
    "foo{#undef}" =? "foo"
    "{#x,he.llo,y}" =? "#1024,Hello%20World!,768"
    "{#path,x}/here" =? "#/foo/bar,1024/here"
    "{#path:6}/here" =? "#/foo/b/here"
    "{#list}" =? "#red,green,blue"
    "{#list*}" =? "#red,green,blue"
    "{#keys}" =? "#semi,;,dot,.,comma,,"
    "{#keys*}" =? "#semi=;,dot=.,comma=,"

// Note - this may seem incorrect, but is actually expected behaviour.
// These are not ideal operators to use for matching complex data!

[<Test>]
let ``Fragment Matching Matches Correctly`` () =
    testMatch "/test{#atom}" "/test#one!" [ Key "atom", Atom "one!" ]
    testMatch "/test{#list*}" "/test#one,two,three" [ Key "list", List [ "one,two,three" ] ]
    testMatch "/test{#keys*}" "/test#one=a,two=b" [ Key "keys", List [ "one=a,two=b" ] ]


//[<Test>]
//let ``Label Expansion with Dot-Prefix Renders Correctly`` () =
//    "{.who}" =? ".fred"
//    "{.who,who}" =? ".fred.fred"
//    "{.half,who}" =? ".50%25.fred"
//    "www{.dom*}" =? "www.example.com"
//    "X{.var}" =? "X.value"
//    "X{.empty}" =? "X."
//    "X{.undef}" =? "X"
//    "X{.var:3}" =? "X.val"
//    "X{.list}" =? "X.red,green,blue"
//    "X{.list*}" =? "X.red.green.blue"
//    "X{.keys}" =? "X.semi,%3B,dot,.,comma,%2C"
//    "X{.keys*}" =? "X.semi=%3B.dot=..comma=%2C"
//    "X{.empty_keys}" =? "X"
//    "X{.empty_keys*}" =? "X"