module Freya.Types.Uri.Tests

open System.Net
open NUnit.Framework
open Freya.Types.Tests
open Freya.Types.Uri

[<Test>]
let ``Scheme Formatting/Parsing`` () =
    let schemeTyped =
        Scheme "http"

    let schemeString =
        "http"

    roundTrip (Scheme.Format, Scheme.Parse) [
        schemeTyped, schemeString ]

[<Test>]
let ``Authority Formatting/Parsing`` () =
    
    (* Host Only *)

    let hostTyped =
        Authority.Authority (IPv4 (IPAddress.Parse "192.168.0.1"), None, None)

    let hostString =
        "192.168.0.1"

    (* Host and Port *)

    let hostPortTyped =
        Authority.Authority (IPv6 (IPAddress.Parse "2001:db8::ff00:42:8329"), Some (Port 8080), None)

    let hostPortString =
        "[2001:db8::ff00:42:8329]:8080"

    (* Host, Port and UserInfo *)

    let hostPortUserTyped =
        Authority.Authority (Name (RegName "www.example.com"), Some (Port 8080),Some (UserInfo "user name:pass"))

    let hostPortUserString =
        "user%20name:pass@www.example.com:8080"

    (* Round Trip *)

    roundTrip (Authority.Format, Authority.Parse) [
        hostTyped,         hostString
        hostPortTyped,     hostPortString
        hostPortUserTyped, hostPortUserString ]

[<Test>]
let ``PathAbsoluteOrEmpty Formatting/Parsing`` () =

    let pathAbEmptyFullTyped = 
        PathAbsoluteOrEmpty [ "some value?"; "path" ]

    let pathAbEmptyFullString =
        "/some%20value%3F/path"

    let pathAbEmptyEmptyTyped = 
        PathAbsoluteOrEmpty []

    let pathAbEmptyEmptyString =
        ""

    roundTrip (PathAbsoluteOrEmpty.Format, PathAbsoluteOrEmpty.Parse) [
        pathAbEmptyFullTyped,  pathAbEmptyFullString
        pathAbEmptyEmptyTyped, pathAbEmptyEmptyString ]

[<Test>]
let ``PathAbsolute Formatting/Parsing`` () =

    let pathAbsoluteFullTyped = 
        PathAbsolute [ "some value?"; "path" ]

    let pathAbsoluteFullString =
        "/some%20value%3F/path"

    let pathAbsoluteEmptyTyped = 
        PathAbsolute []

    let pathAbsoluteEmptyString =
        "/"

    roundTrip (PathAbsolute.Format, PathAbsolute.Parse) [
        pathAbsoluteFullTyped,  pathAbsoluteFullString
        pathAbsoluteEmptyTyped, pathAbsoluteEmptyString ]

[<Test>]
let ``Uri Formatting/Parsing`` () =

    (* Authority Hierarchy *)
    
    let authorityTyped =
        Uri.Uri (
            Scheme "http",
            HierarchyPart.Authority (
                Authority.Authority (Name (RegName "www.example.com"), Some (Port 8080), Some (UserInfo "user name:pass")),
                PathAbsoluteOrEmpty [ "seg1"; "seg2" ]),
            Some (Query "key=some value"),
            Some (Fragment "frag1"))

    let authorityString =
        "http://user%20name:pass@www.example.com:8080/seg1/seg2?key=some%20value#frag1"

    (* Rootless Hierarchy *)

    let rootlessTyped =
        Uri.Uri (
            Scheme "urn",
            HierarchyPart.Rootless (PathRootless [ "example:animal:ferret:nose" ]),
            None,
            None)

    let rootlessString =
        "urn:example:animal:ferret:nose"

    (* Absolute Hierarchy *)

    let absoluteTyped =
        Uri.Uri (
            Scheme "sip",
            HierarchyPart.Absolute (PathAbsolute [ "user"; "example" ]),
            None,
            None)

    let absoluteString =
        "sip:/user/example"

    (* Empty Hierarchy *)

    let emptyTyped =
        Uri.Uri (
            Scheme "test",
            HierarchyPart.Empty,
            None,
            None)

    let emptyString =
        "test:"

    (* Round Trip *)

    roundTrip (Uri.Format, Uri.Parse) [ 
        authorityTyped, authorityString
        rootlessTyped,  rootlessString
        absoluteTyped,  absoluteString
        emptyTyped,     emptyString ]