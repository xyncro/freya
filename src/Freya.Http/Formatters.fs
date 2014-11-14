module internal Freya.Http.Formatters

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

    let contentLength =
        function | ContentLength x -> append (string x)

    (* Section 6 *)

    let private connectionOption =
        function | ConnectionOption x -> append x

    let connection =
        function | Connection x -> join connectionOption comma x


module RFC7231 =

    (* Section 3 *)

    let private pair =
        (<||) (appendf2 "{0}={1}")

    let private parameters =
        function | (x: Map<string, string>) when Map.isEmpty x -> id
                 | (x) -> append ";" >> join pair semicolon (Map.toList x |> List.rev)

    let mediaType =
        function | MediaType (Type x, SubType y, p) -> appendf2 "{0}/{1}" x y >> parameters p

    let private encoding =
        function | Encoding x -> append x

    let contentType =
        function | ContentType x -> mediaType x

    let contentEncoding =
        function | ContentEncoding x -> join encoding comma x

    let expect =
        function | Expect Continue -> append "100-continue"

    let maxForwards =
        function | MaxForwards value -> append (string value)

    (* Section 5 *)

    let private weight =
        function | Some (x: float) -> appendf1 ";q={0:G4}" x
                 | _ -> id

    let query =
        function | (x: Map<string, string>) when Map.isEmpty x -> id
                 | (x) -> join pair ampersand (Map.toList x |> List.rev)

    let private mediaRange =
        function | MediaRange.Closed (Type x, SubType y, p) -> appendf2 "{0}/{1}" x y >> parameters p
                 | MediaRange.Partial (Type x, p) -> appendf1 "{0}/*" x >> parameters p
                 | MediaRange.Open p -> append "*/*" >> parameters p

    // TODO: Proper extensions...

    let private acceptExtensions =
        function | (x: Map<string, string option>) when Map.isEmpty x -> id
                 | _ -> id

    let private acceptParameters =
        function | Some ({ Weight = w; Extensions = e }) -> weight (Some w) >> acceptExtensions e
                 | _ -> id

    let private acceptableMedia (value: AcceptableMedia) =
        mediaRange value.MediaRange >> acceptParameters value.Parameters

    let accept =
        function | Accept x -> join acceptableMedia comma x

    let private charsetSpec =
        function | CharsetSpec.Charset (Charset x) -> append x
                 | CharsetSpec.Any -> append "*"

    let private acceptableCharset (value: AcceptableCharset) =
        charsetSpec value.Charset >> weight value.Weight

    let acceptCharset =
        function | AcceptCharset x -> join acceptableCharset comma x

    let private encodingSpec =
        function | EncodingSpec.Encoding (Encoding.Encoding x) -> append x
                 | EncodingSpec.Identity -> append "identity"
                 | EncodingSpec.Any -> append "*" 

    let private acceptableEncoding x =
        encodingSpec x.Encoding >> weight x.Weight

    let acceptEncoding =
        function | AcceptEncoding x -> join acceptableEncoding comma x

    let private cultureInfo (x: CultureInfo) =
        append x.Name

    let private acceptableLanguage x =
        cultureInfo x.Language >> weight x.Weight

    let acceptLanguage =
        function | AcceptLanguage x -> join acceptableLanguage comma x

    (* Section 7 *)

    let date =
        function | Date.Date x -> append (x.ToString "r")

    let retryAfter =
        function | Date x -> append (x.ToString "r")
                 | Delay x -> append (string x)

    let allow =
        function | Allow x -> join RFC7230.meth comma x


module RFC7232 =

    (* Section 2 *)

    let lastModified =
        function | LastModified x -> append (x.ToString "r")

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

    let ifModifiedSince =
        function | IfModifiedSince x -> append (x.ToString "r")

    let ifUnmodifiedSince =
        function | IfUnmodifiedSince x -> append (x.ToString "r")


module RFC7234 =

    (* Section 5 *)

    let age =
        function | Age x -> append (string x.Seconds)

    let expires =
        function | Expires x -> append (x.ToString "r")
