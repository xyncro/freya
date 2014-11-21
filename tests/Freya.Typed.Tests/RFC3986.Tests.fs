module Freya.Typed.Tests.RFC3986

open System.Net
open NUnit.Framework
open Swensen.Unquote
open Freya.Typed

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
        { Host = IPv4 (IPAddress.Parse "192.168.0.1")
          Port = None
          UserInfo = None }

    let hostString =
        "192.168.0.1"

    (* Host and Port *)

    let hostPortTyped =
        { Host = IPv6 (IPAddress.Parse "2001:db8::ff00:42:8329")
          Port = Some (Port 8080)
          UserInfo = None }

    let hostPortString =
        "[2001:db8::ff00:42:8329]:8080"

    (* Host, Port and UserInfo *)

    let hostPortUserTyped =
        { Host = Name "www.example.com"
          Port = Some (Port 8080)
          UserInfo = Some (UserInfo "user:pass") }

    let hostPortUserString =
        "user:pass@www.example.com:8080"

    (* Round Trip *)

    roundTrip (Authority.Format, Authority.Parse) [
        hostTyped,         hostString
        hostPortTyped,     hostPortString
        hostPortUserTyped, hostPortUserString ]

[<Test>]
let ``PathAbsoluteOrEmpty Formatting/Parsing`` () =

    let pathAbEmptyFullTyped = 
        PathAbsoluteOrEmpty [ "some"; "path" ]

    let pathAbEmptyFullString =
        "/some/path"

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
        PathAbsolute [ "some"; "path" ]

    let pathAbsoluteFullString =
        "/some/path"

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
        { Scheme = Scheme "http"
          Hierarchy = 
            HierarchyPart.Authority ({ Host = Name "www.example.com"
                                       Port = Some (Port 8080)
                                       UserInfo = Some (UserInfo "user:pass") },
                                     PathAbsoluteOrEmpty [ "seg1"; "seg2" ])
          Query = Some (Query "key=val")
          Fragment = Some (Fragment "frag1") }

    let authorityString =
        "http://user:pass@www.example.com:8080/seg1/seg2?key=val#frag1"

    (* Rootless Hierarchy *)

    let rootlessTyped =
        { Scheme = Scheme "urn"
          Hierarchy = HierarchyPart.Rootless (PathRootless [ "example:animal:ferret:nose" ])
          Query = None
          Fragment = None }

    let rootlessString =
        "urn:example:animal:ferret:nose"

    (* Absolute Hierarchy *)

    let absoluteTyped =
        { Scheme = Scheme "sip"
          Hierarchy = HierarchyPart.Absolute (PathAbsolute [ "user"; "example" ])
          Query = None
          Fragment = None }

    let absoluteString =
        "sip:/user/example"

    (* Empty Hierarchy *)

    let emptyTyped =
        { Scheme = Scheme "test"
          Hierarchy = HierarchyPart.Empty
          Query = None
          Fragment = None }

    let emptyString =
        "test:"

    (* Round Trip *)

    roundTrip (Uri.Format, Uri.Parse) [ 
        authorityTyped, authorityString
        rootlessTyped,  rootlessString
        absoluteTyped,  absoluteString
        emptyTyped,     emptyString ]