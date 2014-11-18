module Freya.Http.Tests.RFC3986

open System.Net
open NUnit.Framework
open Swensen.Unquote
open Freya.Http

[<Test>]
let ``Authority parses authority correctly`` () =
    let auth1 = Authority.TryParse "192.168.0.1"

    auth1.IsSome =? true
    auth1.Value.Host =? IPv4 (IPAddress.Parse "192.168.0.1")
    auth1.Value.Port =? None
    auth1.Value.UserInfo =? None

    let auth2 = Authority.TryParse "user:pass@www.example.com"

    auth2.IsSome =? true
    auth2.Value.Host =? RegName ("www.example.com")
    auth2.Value.UserInfo =? Some (UserInfo "user:pass")
    auth2.Value.Port =? None

    let auth3 = Authority.TryParse "user:pass@[2001:db8::ff00:42:8329]:8080"

    auth3.IsSome =? true
    auth3.Value.Host =? IPv6 (IPAddress.Parse "2001:db8::ff00:42:8329")
    auth3.Value.UserInfo =? Some (UserInfo "user:pass")
    auth3.Value.Port =? Some (Port 8080)
    


