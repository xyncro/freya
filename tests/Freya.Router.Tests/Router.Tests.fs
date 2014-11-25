module Freya.Router.Tests.Router

open NUnit.Framework
open Swensen.Unquote
open Freya.Pipeline
open Freya.Router

[<Test>]
let ``Router With No Routes Returns Next`` () =
    result "/" (freyaRouter { return () }) =? Next

