module internal Dyfrig.Http.Formatters

open System.Globalization
open System.Text

(* Formatting *)

let format (formatter: 'a -> StringBuilder -> StringBuilder) =
    fun a -> string (formatter a (StringBuilder ()))

(* Formatting Patterns *)

type private Formatter<'a> =
    'a -> StringBuilder -> StringBuilder

type private Separator =
    StringBuilder -> StringBuilder

let private append (s: string) (b: StringBuilder) =
    b.Append s

let private appendf1 (s: string) (v1: obj) (b: StringBuilder) =
    b.AppendFormat (s, v1)

let private appendf2 (s: string) (v1: obj) (v2: obj) (b: StringBuilder) =
    b.AppendFormat (s, v1, v2)

let private join<'a> (f: Formatter<'a>) (s: Separator) =
    let rec join values (b: StringBuilder) =
        match values with
        | [] -> b
        | h :: [] -> f h b
        | h :: t -> (f h >> s >> join t) b

    join

let private ampersand : Separator =
    fun b -> b.Append "&"

let private comma : Separator =
    fun b -> b.Append ","

let private semicolon : Separator =
    fun b -> b.Append ";"


module Generic =

    let scheme =
        function | HTTP -> append "http"
                 | HTTPS -> append "https"
                 | Scheme.Custom x -> append x


module RFC7230 =

    (* Section 3 *)

    let meth =
        function | DELETE -> append "DELETE" 
                 | HEAD -> append "HEAD" 
                 | GET -> append "GET" 
                 | OPTIONS -> append "OPTIONS"
                 | PATCH -> append "PATCH" 
                 | POST -> append "POST" 
                 | PUT -> append "PUT"  
                 | TRACE -> append "TRACE"
                 | Method.Custom x -> append x

    let httpVersion =
        function | HttpVersion.HTTP x -> appendf1 "HTTP/{0:G4}" x 
                 | HttpVersion.Custom x -> append x

    (* Section 6 *)

    let private connectionOption =
        function | ConnectionOption x -> append x

    let connection (Connection options) =
        join connectionOption comma options


module RFC7231 =

    (* Section 5 *)

    let private pair =
        (<||) (appendf2 "{0}={1}")

    let private parameters =
        function | (x: Map<string, string>) when Map.isEmpty x -> id
                 | x -> join pair semicolon (Map.toList x |> List.rev)

    let private weight =
        function | Some (x: float) -> appendf1 ";q={0:G4}" x
                 | _ -> id

    let query =
        function | (x: Map<string, string>) when Map.isEmpty x -> id
                 | x -> join pair ampersand (Map.toList x |> List.rev)

    let private mediaRangeSpec =
        function | MediaRangeSpec.Closed (Type x, SubType y) -> appendf2 "{0}/{1}" x y
                 | MediaRangeSpec.Partial (Type x) -> appendf1 "{0}/*" x
                 | MediaRangeSpec.Open -> append "*/*"

    let private mediaRange (value: MediaRange) =
        mediaRangeSpec value.MediaRange >> parameters value.Parameters

//    let private mediaTypeSpec (value: MediaTypeSpec) (builder: StringBuilder) =
//        match value with
//        | MediaType (Type x, SubType y) -> builder.AppendFormat ("{0}/{1}", x, y)
//
//    let private mediaType (value: MediaType) =
//        mediaTypeSpec value.MediaType >> parameters value.Parameters

    let private acceptValue (value: AcceptValue) =
        mediaRange value.MediaRange >> weight value.Weight

    let accept (Accept values) =
        join acceptValue comma values

    let private charsetSpec =
        function | CharsetSpec.Charset (Charset x) -> append x
                 | CharsetSpec.Any -> append "*"

    let private acceptCharsetValue (value: AcceptableCharset) =
        charsetSpec value.Charset >> weight value.Weight

    let acceptCharset (AcceptCharset values) =
        join acceptCharsetValue comma values

    let private encodingSpec =
        function | EncodingSpec.Encoding (Encoding.Encoding x) -> append x
                 | EncodingSpec.Identity -> append "identity"
                 | EncodingSpec.Any -> append "*" 

    let private acceptEncodingValue (value: AcceptableEncoding) =
        encodingSpec value.Encoding >> weight value.Weight

    let acceptEncoding (AcceptEncoding values) =
        join acceptEncodingValue comma values

    let private cultureInfo (value: CultureInfo) =
        append value.Name

    let private acceptLanguageValue (value: AcceptableLanguage) =
        cultureInfo value.Language >> weight value.Weight

    let acceptLanguage (AcceptLanguage values) =
        join acceptLanguageValue comma values


module RFC7232 =

    (* Section 2 *)

    let eTag =
        function | Strong x -> appendf1 "\"{0}\"" x
                 | Weak x -> appendf1 "W/\"{0}\"" x

    (* Section 3 *)

    let ifMatch =
        function | IfMatch.EntityTags x -> join eTag comma x
                 | IfMatch.Any -> append "*"

    let ifNoneMatch =
        function | IfNoneMatch.EntityTags x -> join eTag comma x
                 | IfNoneMatch.Any -> append "*"
