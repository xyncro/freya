[<AutoOpen>]
module internal Dyfrig.Http.Formatters

(* RFC 7231 *)

let formatMethod =
    function | DELETE -> "DELETE" 
             | HEAD -> "HEAD" 
             | GET -> "GET" 
             | OPTIONS -> "OPTIONS"
             | PATCH -> "PATCH" 
             | POST -> "POST" 
             | PUT -> "PUT"  
             | TRACE -> "TRACE"
             | Method.Custom x -> x

let formatProtocol =
    function | Protocol.HTTP x -> sprintf "HTTP/%.2g" x 
             | Protocol.Custom x -> x

let formatScheme =    
    function | HTTP -> "http"
             | HTTPS -> "https" 
             | Scheme.Custom x -> x

let formatQuery m =
    Map.toArray m
    |> Array.map (fun (x, y) -> sprintf "%s=%s" x y)
    |> Array.rev
    |> String.concat "&"

(* Media Types/Ranges *)

let private formatParameters (parameters: Map<string, string>) =
    match parameters.Count with
    | 0 -> ""
    | _ ->
        parameters
        |> Map.toArray
        |> Array.map (fun (x, y) -> sprintf "%s=%s" x y)
        |> Array.rev
        |> String.concat ";"
        |> sprintf ";%s"

let formatMediaRange (x: MediaRange) =
    let mediaRange =
        match x.MediaRange with
        | MediaRangeSpec.Closed (Type x, SubType y) -> sprintf "%s/%s" x y
        | MediaRangeSpec.Partial (Type x) -> sprintf "%s/*" x
        | MediaRangeSpec.Open -> "*/*"

    sprintf "%s%s" mediaRange (formatParameters x.Parameters)

let formatMediaType (x: MediaType) =
    let mediaType =
        match x.MediaType with
        | MediaType (Type x, SubType y) -> sprintf "%s/%s" x y

    sprintf "%s%s" mediaType (formatParameters x.Parameters)

let private formatWeight =
    Option.map (sprintf ";q=%.4g") >> Option.getOrElse ""

(* Content Negotiation *)

let formatAccept =
    List.map (fun (x) ->
        sprintf "%s%s" (formatMediaRange x.MediaRange) (formatWeight x.Weight))
    >> String.concat ","

let formatAcceptCharset =
    List.map (fun x ->
        let charset =
            match x.Charset with
            | CharsetSpec.Charset (Charset.Charset x) -> x
            | CharsetSpec.Any -> "*"                    

        sprintf "%s%s" charset (formatWeight x.Weight))
    >> String.concat ","

let formatAcceptEncoding =
    List.map (fun x ->
        let encoding =
            match x.Encoding with
            | EncodingSpec.Encoding (Encoding.Encoding x) -> x
            | EncodingSpec.Identity -> "identity"
            | EncodingSpec.Any -> "*"                    

        sprintf "%s%s" encoding (formatWeight x.Weight)) 
    >> String.concat ","

let formatAcceptLanguage =
    List.map (fun x -> sprintf "%s%s" x.Language.Name (formatWeight x.Weight))
    >> String.concat ","

(* RFC 7232 *)

let formatETag =
    function | Strong x -> sprintf "\"%s\"" x
             | Weak x -> sprintf "W/\"%s\"" x

let formatIfMatch =
    function | IfMatch.EntityTags x -> List.map formatETag x |> String.concat ","
             | IfMatch.Any -> "*"

let formatIfNoneMatch =
    function | IfNoneMatch.EntityTags x -> List.map formatETag x |> String.concat ","
             | IfNoneMatch.Any -> "*"