module Freya.Http.Tests.RFC3986

open System.Net
open NUnit.Framework
open Swensen.Unquote
open Freya.Http

[<Test>]
let ``Authority parses correctly`` () =
    let auth1 = Authority.TryParse "192.168.0.1"

    auth1.IsSome =? true
    auth1.Value.Host =? IPv4 (IPAddress.Parse "192.168.0.1")
    auth1.Value.Port =? None
    auth1.Value.UserInfo =? None

    let auth2 = Authority.TryParse "user:pass@www.example.com"

    auth2.IsSome =? true
    auth2.Value.Host =? Name ("www.example.com")
    auth2.Value.UserInfo =? Some (UserInfo "user:pass")
    auth2.Value.Port =? None

    let auth3 = Authority.TryParse "user:pass@[2001:db8::ff00:42:8329]:8080"

    auth3.IsSome =? true
    auth3.Value.Host =? IPv6 (IPAddress.Parse "2001:db8::ff00:42:8329")
    auth3.Value.UserInfo =? Some (UserInfo "user:pass")
    auth3.Value.Port =? Some (Port 8080)

[<Test>]
let ``Uri parses correctly`` () =
    let uri1 = Uri.Parse "http://user:pass@www.example.com:8080/seg1/seg2"
    uri1.Scheme =? Scheme "http"
    uri1.Hierarchy =? Authority ({ Host = Name "www.example.com"
                                   Port = Some (Port 8080)
                                   UserInfo = Some (UserInfo "user:pass") }, 
                                 PathAbsoluteOrEmpty [ "seg1"; "seg2" ])

    let uri2 = Uri.Parse "urn:example:animal:ferret:nose"
    uri2.Scheme =? Scheme "urn"
    uri2.Hierarchy =? Rootless (PathRootless [ "example:animal:ferret:nose" ])

    let uri3 = Uri.Parse "mailto:fred@example.com"
    uri3.Scheme =? Scheme "mailto"
    uri3.Hierarchy =? Rootless (PathRootless [ "fred@example.com" ])

    let uri4 = Uri.Parse "sip:/user/example"
    uri4.Scheme =? Scheme "sip"
    uri4.Hierarchy =? Absolute (PathAbsolute [ "user"; "example" ])

[<Test>]
let ``Uri fails correctly`` () =
    raises<exn> <@ Uri.Parse "<invalid>" @>