module Freya.Typed.Tests.RFC7231

open System
open NUnit.Framework
open Swensen.Unquote
open Freya.Typed

[<Test>]
let ``MediaType Formatting/Parsing`` () =

    let mediaTypeTyped =
        MediaType (Type "application", SubType "json", Map.ofList [ "charset", "utf-8" ])

    let mediaTypeString =
        "application/json;charset=utf-8"

    roundTrip (MediaType.Format, MediaType.Parse) [
        mediaTypeTyped, mediaTypeString ]

[<Test>]
let ``ContentType Formatting/Parsing`` () =

    let contentTypeTyped =
        ContentType (MediaType (Type "application", SubType "json", Map.ofList [ "charset", "utf-8" ]))

    let contentTypeString =
        "application/json;charset=utf-8"

    roundTrip (ContentType.Format, ContentType.Parse) [
        contentTypeTyped, contentTypeString ]

[<Test>]
let ``ContentEncoding Formatting/Parsing`` () =

    let contentEncodingTyped =
        ContentEncoding [ 
            ContentCoding "compress"
            ContentCoding "deflate" ]

    let contentEncodingString =
        "compress,deflate"

    roundTrip (ContentEncoding.Format, ContentEncoding.Parse) [
        contentEncodingTyped, contentEncodingString ]

[<Test>]
let ``ContentLanguage Formatting/Parsing`` () =

    let contentLanguageTyped =
        ContentLanguage [
            { Language = Language ("en", None)
              Script = None
              Region = Some (Region "GB")
              Variant = Variant [] }
            { Language = Language ("hy", None)
              Script = Some (Script "Latn")
              Region = Some (Region "IT")
              Variant = Variant [ "arvela" ] } ]

    let contentLanguageString =
        "en-GB,hy-Latn-IT-arvela"

    roundTrip (ContentLanguage.Format, ContentLanguage.Parse) [
        contentLanguageTyped, contentLanguageString ]

[<Test>]
let ``ContentLocation Formatting/Parsing`` () =
    
    let contentLocationTyped =
        ContentLocation (
            ContentLocationUri.Absolute ({ Scheme = Scheme "http"
                                           Hierarchy = HierarchyPart.Absolute (PathAbsolute [ "some"; "path" ])
                                           Query = None }))

    let contentLocationString =
        "http:/some/path"

    roundTrip (ContentLocation.Format, ContentLocation.Parse) [
        contentLocationTyped, contentLocationString ]

[<Test>]
let ``Method Formatting/Parsing`` () =

    roundTrip (Method.Format, Method.Parse) [
        Method.GET, "GET"
        Method.Custom "CONNECT", "CONNECT" ]

[<Test>]
let ``Expect Formatting/Parsing`` () =

    roundTrip (Expect.Format, Expect.Parse) [
        Expect (Continue), "100-continue" ]

[<Test>]
let ``MaxForwards Formatting/Parsing`` () =
    
    roundTrip (MaxForwards.Format, MaxForwards.Parse) [
        MaxForwards 10, "10" ]

[<Test>]
let ``Accept Formatting/Parsing`` () =

    let acceptTyped =
        Accept [
            { MediaRange = Closed (Type "application", SubType "json", Map.empty)
              Parameters = Some { Weight = 0.7
                                  Extensions = Map.empty } }
            { MediaRange = MediaRange.Partial (Type "text", Map.empty)
              Parameters = Some { Weight = 0.5
                                  Extensions = Map.empty } }
            { MediaRange = Open (Map.empty)
              Parameters = None } ]

    let acceptString =
        "application/json;q=0.7,text/*;q=0.5,*/*"

    roundTrip (Accept.Format, Accept.Parse) [
        acceptTyped, acceptString ]

[<Test>]
let ``AcceptCharset Formatting/Parsing`` () =
    
    let acceptCharsetTyped =
        AcceptCharset [
            { Charset = CharsetRange.Charset (Charsets.UTF8)
              Weight = Some 0.7 }
            { Charset = CharsetRange.Any
              Weight = Some 0.2 } ]

    let acceptCharsetString =
        "utf-8;q=0.7,*;q=0.2"

    roundTrip (AcceptCharset.Format, AcceptCharset.Parse) [
        acceptCharsetTyped, acceptCharsetString ]

[<Test>]
let ``AcceptEncoding Formatting/Parsing`` () =

    let acceptEncodingTyped =
        AcceptEncoding [
            { Encoding = Coding (ContentCodings.Compress)
              Weight = Some 0.8 }
            { Encoding = Identity
              Weight = None }
            { Encoding = EncodingRange.Any
              Weight = Some 0.3 } ]

    let acceptEncodingString =
        "compress;q=0.8,identity,*;q=0.3"

    roundTrip (AcceptEncoding.Format, AcceptEncoding.Parse) [
        acceptEncodingTyped, acceptEncodingString ]

[<Test>]
let ``AcceptLanguage Formatting/Parsing`` () =

    let acceptLanguageTyped =
        AcceptLanguage [
            { Language = Range [ "en"; "GB" ]
              Weight = Some 0.8 }
            { Language = Any
              Weight = None } ]

    let acceptLanguageString =
        "en-GB;q=0.8,*"

    roundTrip (AcceptLanguage.Format, AcceptLanguage.Parse) [
        acceptLanguageTyped, acceptLanguageString ]

[<Test>]
let ``Referer Formatting/Parsing`` () =

    let refererTyped =
        Referer (Partial ({ Relative = RelativePart.Absolute (PathAbsolute ["some"; "path" ])
                            Query = None }))

    let refererString =
        "/some/path"

    roundTrip (Referer.Format, Referer.Parse) [
        refererTyped, refererString ]

[<Test>]
let ``Date Formatting/Parsing`` () =

    let dateTyped =
        Date.Date (DateTime.Parse ("1994/10/29 19:43:31"))

    let dateString =
        "Sat, 29 Oct 1994 19:43:31 GMT"

    roundTrip (Date.Format, Date.Parse) [
        dateTyped, dateString ]

[<Test>]
let ``Location Formatting/Parsing`` () =

    let locationTyped =
        Location (UriReference.Uri { Scheme = Scheme "http"
                                     Hierarchy = HierarchyPart.Absolute (PathAbsolute [ "some"; "path" ])
                                     Query = None
                                     Fragment = None })

    let locationString =
        "http:/some/path"

    roundTrip (Location.Format, Location.Parse) [
        locationTyped, locationString ]

[<Test>]
let ``RetryAfter Formatting/Parsing`` () =

    let retryAfterTyped =
        RetryAfter (Delay 60)

    let retryAfterString =
        "60"

    roundTrip (RetryAfter.Format, RetryAfter.Parse) [
        retryAfterTyped, retryAfterString ]

[<Test>]
let ``Allow Formatting/Parsing`` () =

    let allowTyped =
        Allow [ DELETE; GET; POST; PUT ]

    let allowString =
        "DELETE,GET,POST,PUT"

    roundTrip (Allow.Format, Allow.Parse) [
        allowTyped, allowString ]